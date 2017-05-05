#import "AppDelegate.h"
#import "ViewController.h"
#import <UserNotifications/UserNotifications.h>

extern void didRegisterForRemoteNotificationsWithDeviceTokenCallback(HsStablePtr, const char * _Nonnull);

@interface AppDelegate ()

@end

HsStablePtr globalHandler = 0;
HsStablePtr globalDeviceTokenHook = 0;

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    // Override point for customization after application launch.
    self.window.rootViewController = [[ViewController alloc] initWithHandler:globalHandler];
    self.window.backgroundColor = [UIColor whiteColor];
    [self.window makeKeyAndVisible];
    UNUserNotificationCenter* center = [UNUserNotificationCenter currentNotificationCenter];
    [center requestAuthorizationWithOptions:(UNAuthorizationOptionAlert + UNAuthorizationOptionSound + UNAuthorizationOptionBadge)
       completionHandler:^(BOOL granted, NSError * _Nullable error) {
         // Handler used to alter application behavior based on types of notifications authorized
         // Perhaps we don't need to do anything here.
    }];
    [application registerForRemoteNotifications];
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
}


- (void)applicationDidEnterBackground:(UIApplication *)application {
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}


- (void)applicationWillEnterForeground:(UIApplication *)application {
    // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}


- (void)applicationWillTerminate:(UIApplication *)application {
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
}

- (void)application:(UIApplication *)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData *)deviceToken {
    NSString *deviceTokenString = [deviceToken base64EncodedStringWithOptions: 0];
	didRegisterForRemoteNotificationsWithDeviceTokenCallback(globalDeviceTokenHook, [deviceTokenString UTF8String]);
}

@end

void runInWKWebView(HsStablePtr handler, const char * _Nonnull progName, HsStablePtr deviceTokenHook) {
    @autoreleasepool {
        globalHandler = handler;
        globalDeviceTokenHook = deviceTokenHook;
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
