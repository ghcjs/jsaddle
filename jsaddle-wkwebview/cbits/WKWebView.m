#include "HsFFI.h"
#import <Foundation/Foundation.h>
#import <WebKit/WebKit.h>

extern void jsaddleStart(HsStablePtr);
extern void jsaddleResult(HsStablePtr, const char *  _Nonnull result);

@interface JSaddleHandler : NSObject<WKNavigationDelegate, WKScriptMessageHandler>

@property (nonatomic, assign) HsStablePtr startHandler;
@property (nonatomic, assign) HsStablePtr resultHandler;

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler;

@end

@implementation JSaddleHandler

- (instancetype)initHandler:(HsStablePtr)startHandler resultHandler:(HsStablePtr)resultHandler
{
    self = [super init];
    if (self) {
        _startHandler = startHandler;
        _resultHandler = resultHandler;
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

@end

void addJSaddleHandler(WKWebView *webView, HsStablePtr startHandler, HsStablePtr resultHandler) {
    JSaddleHandler * handler = [[JSaddleHandler alloc] initHandler:startHandler resultHandler:resultHandler];
    [[[webView configuration] userContentController] addScriptMessageHandler:handler name:@"jsaddle"];
    webView.navigationDelegate = handler;
}

void evaluateJavaScript(WKWebView *webView, const char * _Nonnull js) {
    [webView evaluateJavaScript:[NSString stringWithCString:js encoding:NSUTF8StringEncoding] completionHandler:NULL];
}

void loadHTMLString(WKWebView *webView, const char * _Nonnull html) {
    [webView loadHTMLString:[NSString stringWithCString:html encoding:NSUTF8StringEncoding] baseURL:NULL];
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



