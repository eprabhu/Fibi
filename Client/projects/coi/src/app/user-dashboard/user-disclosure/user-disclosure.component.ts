import {Component} from '@angular/core';
import {UserDisclosureService} from "./user-disclosure.service";
import {UserDashboardService} from "../user-dashboard.service";

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss']
})

export class UserDisclosureComponent {
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
    };
    disclosureArray: any[] = [];
    isActiveDisclosureAvailable = false;

    constructor(public userDisclosureService: UserDisclosureService, public userDashboardService: UserDashboardService) {
    }

    ngOnInit() {
        this.userDisclosureService.getCOIDashboard(this.dashboardRequestObject).subscribe((res: any) => {
            this.disclosureArray = res.disclosureViews ? res.disclosureViews : [];
            console.log(this.disclosureArray);
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
}
