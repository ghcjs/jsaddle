#include "HsFFI.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

extern void callIO(HsStablePtr);
extern void callWithCString(const char * _Nonnull, HsStablePtr);
extern void callWithWebView(WKWebView *, HsStablePtr);

@interface AppDelegate : NSObject <NSApplicationDelegate>
@property (nonatomic, assign) IBOutlet NSWindow *window;
@property (nonatomic, assign) HsStablePtr handler;

- (instancetype)initApp:(HsStablePtr)handler progName:(NSString *)progName;
@end

HsStablePtr global_willFinishLaunchingWithOptions = 0;
HsStablePtr global_didFinishLaunchingWithOptions = 0;
HsStablePtr global_applicationUniversalLink = 0;
uint64_t global_developerExtrasEnabled = 1;

@implementation AppDelegate

- (void)applicationWillTerminate:(NSNotification *)aNotification {
    // Insert code here to tear down your application
}

-(id)initApp:(HsStablePtr)handler progName:(NSString *)progName {
    self = [super init];
    if (self) {
        _handler = handler;
        NSRect contentSize = NSMakeRect(0.0, 500.0, 1000.0, 700.0);
        // TODO replace with once nix has newer Cocoa NSUInteger windowStyleMask = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable;
        NSUInteger windowStyleMask = NSTitledWindowMask | NSResizableWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask;
        _window = [[NSWindow alloc] initWithContentRect:contentSize styleMask:windowStyleMask backing:NSBackingStoreBuffered defer:YES];
        _window.backgroundColor = [NSColor whiteColor];
        _window.title = progName;
    }
    return self;
}

-(void)applicationWillFinishLaunching:(NSNotification *)notification {
    WKWebViewConfiguration *theConfiguration = [[WKWebViewConfiguration alloc] init];
    if (global_developerExtrasEnabled) {
      [theConfiguration.preferences setValue:@YES forKey:@"developerExtrasEnabled"];
    }
    WKWebView *webView = [[WKWebView alloc] initWithFrame: [_window.contentView frame] configuration:theConfiguration];
    [_window setContentView:webView];
    callWithWebView(webView, _handler);
    callIO(global_willFinishLaunchingWithOptions);
    // Listen to URL events
    NSAppleEventManager *manager = [NSAppleEventManager sharedAppleEventManager];
    [manager
      setEventHandler:self
      andSelector:@selector(applicationUniversalLink:)
      forEventClass:kInternetEventClass
      andEventID:kAEGetURL];
}

- (void)applicationUniversalLink:(NSAppleEventDescriptor *)event {
  NSString *url = [[event paramDescriptorForKeyword:keyDirectObject] stringValue];
  callWithCString([url UTF8String], global_applicationUniversalLink);
}

-(void)applicationDidFinishLaunching:(NSNotification *)notification {
    [_window orderFrontRegardless];
    [_window center];
    [NSApp activateIgnoringOtherApps:YES];
    callIO(global_didFinishLaunchingWithOptions);
}

-(BOOL) applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app {
    return YES;
}

@end

void runInWKWebView(HsStablePtr handler,
                    const char * _Nonnull progName,
                    HsStablePtr hs_willFinishLaunchingWithOptions,
                    HsStablePtr hs_didFinishLaunchingWithOptions,
                    HsStablePtr hs_applicationDidBecomeActive,
                    HsStablePtr hs_applicationWillResignActive,
                    HsStablePtr hs_applicationDidEnterBackground,
                    HsStablePtr hs_applicationWillEnterForeground,
                    HsStablePtr hs_applicationWillTerminate,
                    HsStablePtr hs_applicationSignificantTimeChange,
                    HsStablePtr hs_applicationUniversalLink,
                    const uint64_t hs_requestAuthorizationWithOptions,
                    const uint64_t hs_requestAuthorizationOptionBadge,
                    const uint64_t hs_requestAuthorizationOptionSound,
                    const uint64_t hs_requestAuthorizationOptionAlert,
                    const uint64_t hs_requestAuthorizationOptionCarPlay,
                    const uint64_t hs_registerForRemoteNotifications,
                    HsStablePtr hs_didRegisterForRemoteNotificationsWithDeviceToken,
                    HsStablePtr hs_didFailToRegisterForRemoteNotificationsWithError,
                    const uint64_t hs_developerExtrasEnabled) {
    @autoreleasepool {
        global_willFinishLaunchingWithOptions = hs_willFinishLaunchingWithOptions;
        global_didFinishLaunchingWithOptions = hs_didFinishLaunchingWithOptions;
        global_applicationUniversalLink = hs_applicationUniversalLink;
        global_developerExtrasEnabled = hs_developerExtrasEnabled;
        NSApplication *application = [NSApplication sharedApplication];
        AppDelegate *appDelegate = [[AppDelegate alloc] initApp:handler progName:[NSString stringWithCString:progName encoding:NSUTF8StringEncoding]];
        [application setDelegate:appDelegate];
        const char * _Nonnull argv [] =  {"", 0};
        [application setActivationPolicy:NSApplicationActivationPolicyRegular];
        [application
            performSelectorOnMainThread:@selector(run)
            withObject:nil
            waitUntilDone:YES];
    }
}

BOOL openApp(NSURL * url) {
    NSWorkspace *workspace = [NSWorkspace sharedWorkspace];
    return [workspace openURL:url];
}
