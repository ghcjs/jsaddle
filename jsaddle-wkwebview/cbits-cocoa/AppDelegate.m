#include "HsFFI.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

extern void withWebView(WKWebView *, HsStablePtr);

@interface AppDelegate : NSObject <NSApplicationDelegate>
@property (nonatomic, assign) IBOutlet NSWindow *window;
@property (nonatomic, assign) HsStablePtr handler;

- (instancetype)initApp:(HsStablePtr)handler progName:(NSString *)progName;
@end

@implementation AppDelegate

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    // Insert code here to tear down your application
}

-(id)initApp:(HsStablePtr)handler progName:(NSString *)progName {
    self = [super init];
    if (self) {
        _handler = handler;
        NSRect contentSize = NSMakeRect(0.0, 500.0, 1000.0, 700.0);
        NSUInteger windowStyleMask = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable;
        _window = [[NSWindow alloc] initWithContentRect:contentSize styleMask:windowStyleMask backing:NSBackingStoreBuffered defer:YES];
        _window.backgroundColor = [NSColor whiteColor];
        _window.title = progName;
    }
    return self;
}

-(void)applicationWillFinishLaunching:(NSNotification *)notification {
    WKWebViewConfiguration *theConfiguration = [[WKWebViewConfiguration alloc] init];
    [theConfiguration.preferences setValue:@YES forKey:@"developerExtrasEnabled"];
    WKWebView *webView = [[WKWebView alloc] initWithFrame:_window.contentView.frame configuration:theConfiguration];
    [_window setContentView:webView];
    withWebView(webView, _handler);
}

-(void)applicationDidFinishLaunching:(NSNotification *)notification {
    [_window orderFrontRegardless];
    [_window center];
    [NSApp activateIgnoringOtherApps:YES];
}

-(BOOL) applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app {
    return YES;
}

@end

void runInWKWebView(HsStablePtr handler, const char * _Nonnull progName) {
    @autoreleasepool {
        NSApplication *application = [NSApplication sharedApplication];
        AppDelegate *appDelegate = [[AppDelegate alloc] initApp:handler progName:[NSString stringWithCString:progName encoding:NSUTF8StringEncoding]];
        [application setDelegate:appDelegate];
        const char * _Nonnull argv [] =  {"", 0};
        NSApplicationMain(0, argv);
    }
}

