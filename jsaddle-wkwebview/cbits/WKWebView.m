#include "HsFFI.h"
#import <Foundation/Foundation.h>
#import <WebKit/WebKit.h>

BOOL openApp(NSURL * url);

@interface JSaddleHandler : NSObject<WKNavigationDelegate, WKScriptMessageHandler, WKUIDelegate>

@property (nonatomic, assign) HsStablePtr startHandler;
@property (nonatomic, assign) HsStablePtr resultHandler;
@property (nonatomic, assign) HsStablePtr syncHandler;
@property (nonatomic, assign) void (^completionHandler)(NSString *result);

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler syncHandler:(HsStablePtr)syncHandler;

@end

extern void jsaddleStart(HsStablePtr);
extern void jsaddleResult(HsStablePtr, const char *  _Nonnull result);
extern void jsaddleSyncResult(HsStablePtr, JSaddleHandler *, const char * _Nonnull result);

@implementation JSaddleHandler

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler syncHandler:(HsStablePtr)syncHandler
{
    self = [super init];
    if (self) {
        _startHandler = startHandler;
        _resultHandler = resultHandler;
        _syncHandler = syncHandler;
    }
    return self;
}

- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation
{
    jsaddleStart(self.startHandler);
}

- (void)userContentController:(WKUserContentController *)userContentController didReceiveScriptMessage:(WKScriptMessage *)message
{
    NSString * s = (NSString *) message.body;
    jsaddleResult(self.resultHandler, [s UTF8String]);
}

- (void)webView:(WKWebView *)webView runJavaScriptTextInputPanelWithPrompt:(NSString *)prompt
    defaultText:(NSString *)defaultText initiatedByFrame:(WKFrameInfo *)frame completionHandler:(void (^)(NSString *result))completionHandler
{
    if([prompt isEqualToString:@"JSaddleSync"]) {
        _completionHandler = completionHandler;
        jsaddleSyncResult(self.syncHandler, self, [defaultText UTF8String]);
    }
    else {
        // TODO decide what if anything should go here
    }
}

- (void)webView:(WKWebView *)webView decidePolicyForNavigationAction:(WKNavigationAction *)navigationAction decisionHandler:(void (^)(WKNavigationActionPolicy))decisionHandler
{
    NSURL *url = navigationAction.request.URL;

    if(!navigationAction.targetFrame && ![url.scheme isEqualToString:@"file"]) {
        if(openApp(url))
        decisionHandler(WKNavigationActionPolicyCancel);
    else
        decisionHandler(WKNavigationActionPolicyAllow);
    }
    else
        decisionHandler(WKNavigationActionPolicyAllow);
}

@end

void addJSaddleHandler(WKWebView *webView, HsStablePtr startHandler, HsStablePtr resultHandler, HsStablePtr syncHandler) {
    JSaddleHandler * handler = [[JSaddleHandler alloc] initHandler:startHandler resultHandler:resultHandler syncHandler:syncHandler];
    [[[webView configuration] userContentController] addScriptMessageHandler:handler name:@"jsaddle"];
#ifdef USE_UIKIT
    webView.scrollView.bounces = NO;
#endif
    webView.navigationDelegate = handler;
    webView.UIDelegate = handler;
}

void evaluateJavaScript(WKWebView *webView, const char * _Nonnull js) {
    NSString *jsString = [[NSString alloc] initWithCString:js encoding:NSUTF8StringEncoding];
    dispatch_async(dispatch_get_main_queue(), ^{
        [webView evaluateJavaScript:jsString completionHandler:NULL];
        [jsString release];
    });
}

void completeSync(JSaddleHandler *handler, const char * _Nonnull s) {
    NSString *string = [[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding];
    handler.completionHandler(string);
    handler.completionHandler = NULL;
}

void loadHTMLStringWithBaseURL(WKWebView *webView, const char * _Nonnull html, const char * _Nonnull url) {
  NSString *htmlString = [NSString stringWithCString:html encoding:NSUTF8StringEncoding];
  NSURL *baseURL = [NSURL fileURLWithPath:[[NSString alloc] initWithCString:url encoding:NSUTF8StringEncoding]];
  dispatch_async(dispatch_get_main_queue(), ^{
      [webView loadHTMLString:htmlString baseURL:baseURL];
    });
}

const char * mainBundleResourcePathC() {
    NSBundle *main = [NSBundle mainBundle];
    if(!main) return NULL;
    NSString *path = main.resourcePath;
    if(!path) return NULL;
    return [path UTF8String];
}

void loadBundleFile(WKWebView *webView, const char * _Nonnull file, const char * _Nonnull allowing) {
    NSBundle *main = [NSBundle mainBundle];
    if(main) {
        NSString *path = main.resourcePath;
        if(path) {
            NSURL *fileUrl = [NSURL fileURLWithPath:[NSString stringWithFormat: @"%@/%@", path, [NSString stringWithCString:file encoding:NSUTF8StringEncoding]]];
            NSURL *allowingUrl = [NSURL fileURLWithPath:[NSString stringWithFormat: @"%@/%@", path, [NSString stringWithCString:allowing encoding:NSUTF8StringEncoding]]];
            [webView loadFileURL:fileUrl allowingReadAccessToURL:allowingUrl];
        }
    }
}
