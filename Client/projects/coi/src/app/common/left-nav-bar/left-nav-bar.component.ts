import {Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {environment} from '../../../environments/environment';
import {CommonService} from "../services/common.service";
import {Router} from "@angular/router";
import { COI_CONFIGURATIONS_RIGHTS, ENTITY_RIGHTS } from "../../app-constants";

@Component({
    selector: 'app-left-nav-bar',
    templateUrl: './left-nav-bar.component.html',
    styleUrls: ['./left-nav-bar.component.scss']
})
export class LeftNavBarComponent implements OnInit {
    deployMap = environment.deployUrl;
    isNavExpanded = false;
    canViewAdminDashboard = false;
    canOpenEntity = false;
    canViewFCOIDashboard = false;
    canViewOPADashboard = false;
    @ViewChild('sideBarMenu', {static: true}) sideBarMenu: ElementRef;

    constructor(private _commonService: CommonService, public _router: Router) {
        document.addEventListener('mouseup', this.offClickSideBarHandler.bind(this));
    }

    ngOnInit() {
        this.checkUserHasRight();
    }

    offClickSideBarHandler(event) {
        if (this.isNavExpanded) {
            this.toggleNavBar(!this.isNavExpanded);
        }
    }

    toggleNavBar(quickClose = false) {
        if (quickClose) {
            this.isNavExpanded = !this.isNavExpanded;
        } else {
            setTimeout(() => { this.isNavExpanded = !this.isNavExpanded; }, 200);
        }
    }

    checkUserHasRight(): void {
        this.canOpenEntity = this._commonService.getAvailableRight(ENTITY_RIGHTS);
        this.canViewAdminDashboard = this._commonService.getAvailableRight(COI_CONFIGURATIONS_RIGHTS);
        this.canViewFCOIDashboard = this._commonService.checkFCOIRights();
        this.canViewOPADashboard = this._commonService.checkOPARights();
    }

    scrollToTop(): void {
        window.scrollTo(0,0);
    }
}
