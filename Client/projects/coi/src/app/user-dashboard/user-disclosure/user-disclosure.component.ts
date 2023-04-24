import {Component} from '@angular/core';
import {UserDisclosureService} from "./user-disclosure.service";
import {UserDashboardService} from "../user-dashboard.service";

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss']
})

export class UserDisclosureComponent {
    searchText = '';
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'ALL'
    }
    dashboardRequestObject = {
        advancedSearch: 'L',
        pageNumber: 30,
        sort: {'createTimestamp': 'asc'},
        tabName: 'PROPOSAL_DISCLOSURES',
        isDownload: false,
        // filterType = 'All', 'FCOI', 'Project', 'OPA';
        filterType: 'All'
    };
    disclosureArray: any[] = [];
    filteredDisclosureArray: any[] = [];
    dashboardCount: any;
    isActiveDisclosureAvailable = false;

    constructor(public userDisclosureService: UserDisclosureService, public userDashboardService: UserDashboardService) {
    }

    ngOnInit() {
       this.loadDashboard();
       this.loadDashboardCount();
    }

    loadDashboard() {
        this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject).subscribe((res: any) => {
            this.disclosureArray = res.disclosureViews ? res.disclosureViews : [];
            console.log(this.disclosureArray);
            this.searchText = '';
            this.setFilter();
        })
    }

    loadDashboardCount() {
        this.userDisclosureService.getCOIDashboardCount(this.dashboardRequestObject).subscribe((res: any) => {
            this.dashboardCount = res;
        })
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case 1: return 'warning';
            case 2:
            case 3: return 'success';
            default: return 'info';
        }
    }
    getDispositinTextColor(statusCode) {
        switch (statusCode) {
            case 1: return 'black';
            case 2:
            case 3: return 'white';
            default: return 'white';
        }
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            default: return 'danger';
        }
    }
    getReviewStatusTextColor(statusCode) {
        switch (statusCode) {
            case '1': return 'black';
            case '2': return 'white';
            case '3': return 'white';
            default: return 'white ';
        }
    }

 


    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case 1: return 'warning';
            case 2:
            case 4:
            case 5:
                return 'info';
            case 3: case 6: return 'success';
            default: return 'danger';
        }
    }
    getDisclosureStatusText(statusCode) {
        switch (statusCode) {
            case 1: return 'black';
            case 2:
            case 4:
            case 5: case 3: case 6: return 'white';
            default: return 'black';
        }
    }

    getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType == 1) {
            if (disclosureSequenceStatusCode == 2 || disclosureSequenceStatusCode == 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType == 3) {
            return 'Proposal';
        }
    }

    setTab(tabName) {
        this.currentSelected.tab= tabName;
        this.dashboardRequestObject.tabName = tabName;
        this.loadDashboard();
    }

    setFilter(type = 'ALL') {
        this.currentSelected.filter = type;
        this.dashboardRequestObject.filterType = type;
        this.filterDashboardData();
    }

    filterDashboardData() {
        if (this.currentSelected.filter == 'ALL') {
            this.filteredDisclosureArray = this.disclosureArray;
        } else {
            this.filteredDisclosureArray = this.disclosureArray.filter(disclosure => {
                switch(this.currentSelected.filter) {
                    case 'PROJECT':
                        return ['3', '5'].includes(disclosure.disclosureCategoryTypeCode);
                    case 'OPA':
                        return disclosure.disclosureCategoryTypeCode == '2';
                    case 'FCOI':
                        return disclosure.disclosureCategoryTypeCode == '1';
                }
            });
        }
    }
}
