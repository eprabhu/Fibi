import {Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {environment} from '../../../environments/environment';
import {CommonService} from "../services/common.service";
import {Router} from "@angular/router";
import {ADMIN_DASHBOARD_RIGHTS, COI_DISCLOSURE_SUPER_ADMIN_RIGHTS} from "../../app-constants";

@Component({
    selector: 'app-left-nav-bar',
    templateUrl: './left-nav-bar.component.html',
    styleUrls: ['./left-nav-bar.component.scss']
})
export class LeftNavBarComponent implements OnInit {
    deployMap = environment.deployUrl;
    isNavExpanded = false;
    isAdministrator = false;
    isOPAAdministrator = false;
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
        // FCOI
        this.isAdministrator = this._commonService.getAvailableRight(COI_DISCLOSURE_SUPER_ADMIN_RIGHTS)
            || this._commonService.isCoiReviewer || this._commonService.rightsArray.some((right) => ADMIN_DASHBOARD_RIGHTS.has(right));
        // OPA
        this.isOPAAdministrator = this._commonService.getAvailableRight(['OPA_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_OPA'])
            || this._commonService.isOPAReviewer || this._commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE', 'VIEW_OPA_DISCLOSURE']);
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
        this.canOpenEntity = this._commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY', 'MANAGE_ENTITY_SPONSOR', 'MANAGE_ENTITY_ORGANIZATION', 'MANAGE_ENTITY_COMPLIANCE', 'VERIFY_ENTITY'], 'SOME');
        this.canViewAdminDashboard = this._commonService.getAvailableRight(['APPLICATION_ADMINISTRATOR',
            'MAINTAIN_QUESTIONNAIRE', 'MAINTAIN_USER_ROLES', 'MAINTAIN_ROLE', 'MAINTAIN_PERSON', 'MAINTAIN_TRAINING',
            'VIEW_KEY_PERSON_TIMESHEET', 'MAINTAIN_KEY_PERSON_TIMESHEET', 'MAINTAIN_DELEGATION', 'MAINTAIN_ORCID_WORKS'],
            'SOME');
        this.canViewFCOIDashboard = this._commonService.getAvailableRight([
            'MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE', 'VIEW_PROJECT_DISCLOSURE',
            'MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE', 'MANAGE_CONSULTING_DISCLOSURE',
            'VIEW_CONSULTING_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE_OVERVIEW'],
            'SOME') || this._commonService.isCoiReviewer;
        this.canViewOPADashboard = this._commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE', 'VIEW_OPA_DISCLOSURE'], 'SOME') || this._commonService.isOPAReviewer;
    }

    scrollToTop(): void {
        window.scrollTo(0,0);
    }
}
