
import { throwError as observableThrowError, Observable, from } from 'rxjs';
/**
 * custom request handler for fibi
 * Author Mahesh Sreenath V M
 * this will intercept every request sent from application to Server.
 */
import { Injectable } from '@angular/core';
import { HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpErrorResponse } from '@angular/common/http';
import { catchError, finalize } from 'rxjs/operators';

import { Router } from '@angular/router';
import { CommonService } from './common.service';
import {SSO_TIMEOUT_ERROR_CODE} from "../../../../../fibi/src/app/app-constants";
import { openCommonModal } from '../utilities/custom-utilities';
/**
 * this is used to add authorization token and handle error on token expiration
 * and loader for the entire application is handled here
 * we show the loader on every request start and switch off the loader on request finish or Error.
 * For WAF related issues the Loader is not switched off automatically
 * if the isManualLoader is switched on.See common.service.ts documentation for more details
 * https://docs.google.com/document/d/1x-__S6RpPgnbkS0VmPEyVO3kqtLGit1VFgzTtpImBFs/edit?usp=sharing
 * the scenarios for SSO and Non- SSo are handled differently
 * in SSO environments we can't navigate the user to our login page so the app will use a
 * hard reload since the app login is called on behind the scene with current user
 * the JWT token will be regenerated.But on Non-SSO environments we clear current user credentials
 * and navigates him to login screen. In production most cases we uses SSO.
 */
@Injectable()
export class AppHttpInterceptor implements HttpInterceptor {
    
    AuthToken: string;
    currentActiveAPICount = 0;
    loaderRestrictedUrls: any[] = [];

    constructor(private _router: Router, private _commonService: CommonService) { }
    
    /**catches every request and adds the authentication token from local storage
     * creates new header with auth-key
    */
    intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
        if (this._commonService.isPreventDefaultLoader) {
            this.loaderRestrictedUrls.push(req.urlWithParams);
        } else {
            this.currentActiveAPICount++;
        }
        if (!this._commonService.isPreventDefaultLoader) {
            this._commonService.isShowLoader.next(true);
        }
        return next.handle(req).pipe(
            catchError((error) => {
                if (!this._commonService.isManualLoaderOn) {
                    this._commonService.isShowLoader.next(true);
                    this._commonService.appLoaderContent = 'Loading...';
                    this._commonService.isShowOverlay = false;
                }
                if (error.status === 401 ) {
                    this._commonService.currentUserDetails = {};
                    if(this._commonService.enableSSO) {
                        window.location.reload();
                    } else {
                        this._commonService.removeUserDetailsFromLocalStorage();
                        this._router.navigate(['/login']);
                    }
                }

                if (error.status === 403 && !window.location.href.includes('/login')) {
                    this._router.navigate(['/coi/error-handler/403']);
                }

                if (error.status === SSO_TIMEOUT_ERROR_CODE && this._commonService.enableSSO) {
                    openCommonModal('sessionTimeoutModal');
                }
                if (error.error instanceof Blob) {
                    return from(Promise.resolve(error).then(async x => {
                        throw new HttpErrorResponse({ error: JSON.parse(await x.error.text()), headers: x.headers,
                            status: x.status, statusText: x.statusText, url: x.url || undefined });
                    }));
                }
                return observableThrowError(error);
            }),

            finalize(() => {
                if (this.loaderRestrictedUrls.includes(req.urlWithParams)){
                    this.loaderRestrictedUrls = this.loaderRestrictedUrls.filter(url => url !== req.urlWithParams);
                } else {
                    this.currentActiveAPICount--;
                }
                if (this.currentActiveAPICount <= 0) {
                    this._commonService.isShowLoader.next(false);
                    this._commonService.appLoaderContent = 'Loading...';
                }
            })) as any;
    }

}
