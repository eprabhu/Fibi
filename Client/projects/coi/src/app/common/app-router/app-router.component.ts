import {Component, ElementRef, HostListener, OnDestroy, OnInit} from '@angular/core';
import {CommonService} from "../services/common.service";
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { NavigationEnd, Router } from '@angular/router';
import { focusElementById } from '../utilities/custom-utilities';
import { SSO_LOGOUT_URL, SSO_TIMEOUT_ERROR_MESSAGE } from '../../app-constants';

@Component({
    selector: 'app-app-router',
    templateUrl: './app-router.component.html',
    styleUrls: ['./app-router.component.scss']
})
export class AppRouterComponent implements OnInit, OnDestroy {

    isShowLoader = false;
    $subscriptions = [];
    errorMessage = SSO_TIMEOUT_ERROR_MESSAGE;


    constructor(public commonService: CommonService, private elementRef: ElementRef, private _router: Router) {
        this._router.events.subscribe((event: any) => {
            if (event instanceof NavigationEnd) {
                focusElementById('app-main-router');
            }
        });
    }

    ngOnInit(): void {
        this.$subscriptions.push(this.commonService.isShowLoader.subscribe(data =>
            setTimeout(() => {
                this.isShowLoader = data;
            }, 0)));
    }

    redirectTo(applicationName: 'fibi' | 'COI') {
        window.location.href = this.commonService[applicationName + 'Url'];
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    @HostListener('window:scroll', ['$event'])
    scrollEvent(event) {
        const pageYOffset = this.elementRef.nativeElement.querySelector('.canvas').scrollTop;
        this.commonService.$ScrollAction.next({event, pageYOffset});
    }

    redirectToLogoutPage() {
        localStorage.clear();
        window.location.href = SSO_LOGOUT_URL;
      }
}
