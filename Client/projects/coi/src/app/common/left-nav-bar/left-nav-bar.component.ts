import {Component, ElementRef, OnInit, ViewChild} from '@angular/core';
import {environment} from '../../../environments/environment';
import {CommonService} from "../services/common.service";
import {Router} from "@angular/router";

@Component({
    selector: 'app-left-nav-bar',
    templateUrl: './left-nav-bar.component.html',
    styleUrls: ['./left-nav-bar.component.scss']
})
export class LeftNavBarComponent implements OnInit {
    deployMap = environment.deployUrl;
    isNavExpanded = false;
    isAdministrator = false;
    @ViewChild('sideBarMenu', {static: true}) sideBarMenu: ElementRef;

    constructor(private _commonService: CommonService, private _router: Router) {
        document.addEventListener('mouseup', this.offClickSideBarHandler.bind(this));
    }

    ngOnInit() {
        this.isAdministrator = this._commonService.getAvailableRight(['COI_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_COI'])
            || this._commonService.isCoiReviewer;
        if (this.isAdministrator) {
            this._router.navigate(['/coi/admin-dashboard'], { queryParamsHandling: 'preserve' });
        }
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
}
