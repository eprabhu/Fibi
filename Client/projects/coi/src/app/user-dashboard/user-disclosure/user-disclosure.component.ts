import {Component} from '@angular/core';
import {UserDisclosureService} from "./user-disclosure.service";

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

    constructor(private _service: UserDisclosureService) {
    }

    ngOnInit() {
        this._service.getCOIDashboard(this.dashboardRequestObject).subscribe((res: any) => {
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

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            default: return 'danger';
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
