#include "HsFFI.h"
#import <UIKit/UIKit.h>
#import <WebKit/WebKit.h>

@interface ViewController : UIViewController
@property (nonatomic, assign) HsStablePtr handler;
@property (nonatomic, assign) WKWebView *webView;

- (instancetype)initWithHandler:(HsStablePtr)handler;
@end
