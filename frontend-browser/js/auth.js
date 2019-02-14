// auth.js

class AppAuth {
    constructor() {
        var self = this;
        self.webAuth = new auth0.WebAuth({
            domain: 'mtesseract.eu.auth0.com',
            clientID: '0isBmlwffqd7xTLg0voKLvR4LGGkMbNQ',
            responseType: 'token id_token',
            scope: 'openid profile email',
            redirectUri: window.location.href
        });
        this.localLogin = function (authResult) {
            // Set isLoggedIn flag in localStorage
            // localStorage.setItem('isLoggedIn', 'true');
            // Set the time that the access token will expire at
            // expiresAt = JSON.stringify(
            //   authResult.expiresIn * 1000 + new Date().getTime()
            // );
            // accessToken = authResult.accessToken;
            // id Token = authResult.idToken;
            self.webAuth.client.userInfo(authResult.accessToken, function (err, user) {
                if (self.signInCallback == null) {
                    console.log("No signInCallback registered");
                } else {
                    self.webAuth.client.userInfo(authResult.accessToken, function (err, user) {
                        if (err) {
                            console.log(err);
                        } else if (user == null) {
                            console.log("user object null");
                        } else {
                            console.log(user);
                            self.signInCallback(user.name);
                        }
                    });
                }
            });
        }

        this.registerSignInCallback = function (cb) {
            self.signInCallback = cb;
        }

        this.registerSignOutCallback = function (cb) {
            self.signOutCallback = cb;
        }

        this.signOut = function () {
            if (self.signOutCallback == null) {
                console.log("signOutCallback is null");
            } else {
                self.signOutCallback();
            }
        }

        this.tryRetrieveTokenFromURI = function () {
            self.webAuth.parseHash({ hash: window.location.hash }, function (err, authResult) {
                if (authResult && authResult.accessToken && authResult.idToken) {
                    window.location.hash = '';
                    console.log(authResult);
                    self.localLogin(authResult);
                } else if (err) {
                    return console.log(err);
                }
            });
        }
    }
}

appAuth = new AppAuth();

    // window.addEventListener('load', function () {
    //     var idToken;
    //     var accessToken;
    //     var expiresAt;

    //     var webAuth = 

    //     var loginBtn = document.getElementById('btn-login');

    //     loginBtn.addEventListener('click', function (e) {
    //         e.preventDefault();
    //         webAuth.authorize();
    //     });
