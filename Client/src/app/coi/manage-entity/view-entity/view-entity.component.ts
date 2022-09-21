import { Component, OnInit } from '@angular/core';

@Component({
    selector: 'app-view-entity',
    templateUrl: './view-entity.component.html',
    styleUrls: ['./view-entity.component.css']
})
export class ViewEntityComponent implements OnInit {

    currentDashboardTab = 'CURRENT_DISCLOSURES';

    constructor() { }

    ngOnInit() {
    }

    noDataText() {
        switch (this.currentDashboardTab) {
            case 'CURRENT_DISCLOSURES': {
                return 'There is no current disclosure.';
            }
            case 'PROPOSAL_DISCLOSURES': {
                return 'There are no proposal disclosures.';
            }
            case 'TRAVEL_DISCLOSURES': {
                return 'There are no travel disclosures.';
            }
            case 'SIGNIFICANT_FINANCIAL_INTERESTS': {
                return 'There are no significant financial interests.';
            }
        }
    }

    getButtonLabel() {
        switch (this.currentDashboardTab) {
            case 'CURRENT_DISCLOSURES': {
                return 'Create New FCOI Disclosure';
            }
            case 'PROPOSAL_DISCLOSURES': {
                return 'Create New Proposal Disclosure';
            }
            case 'TRAVEL_DISCLOSURES': {
                return 'Create New Travel Disclosure ';
            }
        }
    }

}
