import {Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {environment} from '../../../environments/environment';
import {CommonService} from "../services/common.service";
import {Router} from "@angular/router";
import {ADMIN_DASHBOARD_RIGHTS} from "../../app-constants";

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
    isManageEntity = false;
    @ViewChild('sideBarMenu', {static: true}) sideBarMenu: ElementRef;

    constructor(private _commonService: CommonService, public _router: Router) {
        document.addEventListener('mouseup', this.offClickSideBarHandler.bind(this));
    }

    ngOnInit() {
        this.checkUserHasRight();
        this.isAdministrator = this._commonService.getAvailableRight(['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'])
            || this._commonService.isCoiReviewer;
        this.isOPAAdministrator = this._commonService.getAvailableRight(['OPA_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_OPA'])
            || this._commonService.isOPAReviewer;
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
        this.isManageEntity = this._commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY'], 'SOME');
        this.canViewAdminDashboard = this._commonService.getAvailableRight(['APPLICATION_ADMINISTRATOR',
                'MAINTAIN_QUESTIONNAIRE', 'MAINTAIN_USER_ROLES', 'MAINTAIN_ROLE', 'MAINTAIN_PERSON', 'MAINTAIN_TRAINING',
                'VIEW_KEY_PERSON_TIMESHEET', 'MAINTAIN_KEY_PERSON_TIMESHEET', 'MAINTAIN_DELEGATION', 'MAINTAIN_ORCID_WORKS'],
            'SOME');
    }
}
