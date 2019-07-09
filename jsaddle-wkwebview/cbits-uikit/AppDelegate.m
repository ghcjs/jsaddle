#import "AppDelegate.h"
#import "ViewController.h"
#import <UserNotifications/UserNotifications.h>
#import <stdint.h>

extern void callIO(HsStablePtr);
extern void callWithCString(const char * _Nonnull, HsStablePtr);

@interface AppDelegate ()

@end

HsStablePtr globalHandler = 0;
HsStablePtr global_willFinishLaunchingWithOptions = 0;
HsStablePtr global_didFinishLaunchingWithOptions = 0;
HsStablePtr global_applicationDidBecomeActive = 0;
HsStablePtr global_applicationWillResignActive = 0;
HsStablePtr global_applicationDidEnterBackground = 0;
HsStablePtr global_applicationWillEnterForeground = 0;
HsStablePtr global_applicationWillTerminate = 0;
HsStablePtr global_applicationSignificantTimeChange = 0;
HsStablePtr global_applicationUniversalLink = 0;
uint64_t global_requestAuthorizationWithOptions = 0;
uint64_t global_requestAuthorizationOptionBadge = 0;
uint64_t global_requestAuthorizationOptionSound = 0;
uint64_t global_requestAuthorizationOptionAlert = 0;
uint64_t global_requestAuthorizationOptionCarPlay = 0;
uint64_t global_registerForRemoteNotifications = 0;
HsStablePtr global_didRegisterForRemoteNotificationsWithDeviceToken = 0;
HsStablePtr global_didFailToRegisterForRemoteNotificationsWithError = 0;

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application willFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Tells the delegate that the launch process has begun but that state restoration has not yet occurred.
    callIO(global_willFinishLaunchingWithOptions);
    return YES;
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Tells the delegate that the launch process is almost done and the app is almost ready to run.
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    // Override point for customization after application launch.
    self.window.rootViewController = [[ViewController alloc] initWithHandler:globalHandler];
    self.window.backgroundColor = [UIColor whiteColor];
    [self.window makeKeyAndVisible];
    UNUserNotificationCenter* center = [UNUserNotificationCenter currentNotificationCenter];
    center.delegate = self;
    if (global_requestAuthorizationWithOptions) {
      UNAuthorizationOptions options = (UNAuthorizationOptions)0;
      if (global_requestAuthorizationOptionBadge) {
        options = options | UNAuthorizationOptionBadge;
      }
      if (global_requestAuthorizationOptionSound) {
        options = options | UNAuthorizationOptionSound;
      }
      if (global_requestAuthorizationOptionAlert) {
        options = options | UNAuthorizationOptionAlert;
      }
      if (global_requestAuthorizationOptionCarPlay) {
        options = options | UNAuthorizationOptionCarPlay;
      }
      [center requestAuthorizationWithOptions:(options)
         completionHandler:^(BOOL granted, NSError * _Nullable error) {
           // Handler used to alter application behavior based on types of notifications authorized
      }];
      if (global_registerForRemoteNotifications) {
        [application registerForRemoteNotifications];
      }
    }
    callIO(global_didFinishLaunchingWithOptions);
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    callIO(global_applicationWillResignActive);
}


- (void)applicationDidEnterBackground:(UIApplication *)application {
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    callIO(global_applicationDidEnterBackground);
}


- (void)applicationWillEnterForeground:(UIApplication *)application {
    // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
    callIO(global_applicationWillEnterForeground);
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    callIO(global_applicationDidBecomeActive);
}


- (void)applicationWillTerminate:(UIApplication *)application {
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    callIO(global_applicationWillTerminate);
}

- (void)application:(UIApplication *)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData *)deviceToken {
    // Tells the delegate that the app successfully registered with Apple Push Notification service (APNs).
    NSString *deviceTokenString = [deviceToken base64EncodedStringWithOptions: 0];
    callWithCString([deviceTokenString UTF8String], global_didRegisterForRemoteNotificationsWithDeviceToken);
}

- (void)application:(UIApplication *)application didReceiveRemoteNotification:(NSDictionary *)userInfo fetchCompletionHandler:(void (^)(UIBackgroundFetchResult result))completionHandler {
    // Sent when the application receives remote notifications in the foreground or background
    // TODO Allow a configurable CString -> IO () to be passed into AppDelegateConfig
    if ([userInfo valueForKeyPath:@"aps.badge"] != nil) {
        [UIApplication sharedApplication].applicationIconBadgeNumber=[[[userInfo objectForKey:@"aps"] objectForKey:@"badge"] intValue];
    }
    completionHandler(UIBackgroundFetchResultNewData);
}

- (void)application:(UIApplication *)application didFailToRegisterForRemoteNotificationsWithError:(NSError *)error {
    // Sent to the delegate when Apple Push Notification service cannot successfully complete the registration process.
    NSString *errorString = [error localizedDescription];
    callWithCString([errorString UTF8String], global_didFailToRegisterForRemoteNotificationsWithError);
}

- (void)applicationSignificantTimeChange:(UIApplication *)application {
    // Tells the delegate when there is a significant change in the time.
    callIO(global_applicationSignificantTimeChange);
}

- (BOOL)application:(UIApplication *)application continueUserActivity:(NSUserActivity *)userActivity restorationHandler:(void (^)(NSArray *restorableObjects))restorationHandler {
    // TODO: Reroute universal links when they're clicked in-app.
    // https://developer.apple.com/reference/webkit/wknavigationdelegate/1455643-webview?language=objc
    if ([userActivity.activityType isEqualToString:NSUserActivityTypeBrowsingWeb] && userActivity.webpageURL) {
        callWithCString([userActivity.webpageURL.absoluteString UTF8String], global_applicationUniversalLink);
        return YES;
    }
    return NO;
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
        globalHandler = handler;
        global_willFinishLaunchingWithOptions = hs_willFinishLaunchingWithOptions;
        global_didFinishLaunchingWithOptions = hs_didFinishLaunchingWithOptions;
        global_applicationDidBecomeActive = hs_applicationDidBecomeActive;
        global_applicationWillResignActive = hs_applicationWillResignActive;
        global_applicationDidEnterBackground = hs_applicationDidEnterBackground;
        global_applicationWillEnterForeground = hs_applicationWillEnterForeground;
        global_applicationWillTerminate = hs_applicationWillTerminate;
        global_applicationSignificantTimeChange = hs_applicationSignificantTimeChange;
        global_applicationUniversalLink = hs_applicationUniversalLink;
        global_requestAuthorizationWithOptions = hs_requestAuthorizationWithOptions;
        global_requestAuthorizationOptionBadge = hs_requestAuthorizationOptionBadge;
        global_requestAuthorizationOptionSound = hs_requestAuthorizationOptionSound;
        global_requestAuthorizationOptionAlert = hs_requestAuthorizationOptionAlert;
        global_requestAuthorizationOptionCarPlay = hs_requestAuthorizationOptionCarPlay;
        global_registerForRemoteNotifications = hs_registerForRemoteNotifications;
        global_didRegisterForRemoteNotificationsWithDeviceToken = hs_didRegisterForRemoteNotificationsWithDeviceToken;
        global_didFailToRegisterForRemoteNotificationsWithError = hs_didFailToRegisterForRemoteNotificationsWithError;
        const char * _Nonnull argv [] =  {"", 0};
        UIApplicationMain(0, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}

BOOL openApp(NSURL * url) {
    UIApplication *app = [UIApplication sharedApplication];
    if ([app canOpenURL:url]) {
        [app openURL:url];
        return true;
    }
    return false;
}
