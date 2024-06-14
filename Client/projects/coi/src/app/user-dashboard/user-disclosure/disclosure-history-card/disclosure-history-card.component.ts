import { Component, Input, OnInit } from '@angular/core';
import { UserDisclosure } from '../user-disclosure-interface';
import { CommonService } from '../../../common/services/common.service';
import { CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, CREATE_DISCLOSURE_ROUTE_URL, POST_CREATE_DISCLOSURE_ROUTE_URL, OPA_REDIRECT_URL, CONSULTING_REDIRECT_URL } from '../../../app-constants';
import { Router } from '@angular/router';
import { getPersonLeadUnitDetails } from '../../../common/utilities/custom-utilities';

@Component({
    selector: 'app-disclosure-history-card',
    templateUrl: './disclosure-history-card.component.html',
    styleUrls: ['./disclosure-history-card.component.scss']
})
export class DisclosureHistoryCardComponent implements OnInit {

    @Input() disclosure: any;
    @Input() isLastElement: any;
    readMoreOrLess = [];
    isPurposeReadMore = false;

    constructor(public commonService: CommonService, private _router: Router) { }

    ngOnInit() { }

    getColorBadges(disclosure: UserDisclosure): string {
        if (disclosure?.travelDisclosureId) {
            return 'bg-travel-clip';
        }
        if (disclosure?.opaDisclosureId) {
            return 'bg-opa-clip';
        }
        if (disclosure.consultDisclId){
            return 'bg-consulting-clip';
        }
        switch (disclosure.fcoiTypeCode) {
            case '1':
                return 'bg-fcoi-clip';
            case '2':
                return 'bg-proposal-clip';
            case '3':
                return 'bg-award-clip';
            default:
                return;
        }
    }

    getProjectTitle(): string {
        if (this.disclosure.fcoiTypeCode === '2' || this.disclosure.fcoiTypeCode === '3') {
            return `#${this.disclosure.projectNumber} - ${this.disclosure.projectTitle}`;
        }
    }

    redirectToDisclosure(): void {
        const redirectUrl = this.disclosure.travelDisclosureId ? CREATE_TRAVEL_DISCLOSURE_ROUTE_URL :
        this.disclosure.opaDisclosureId ? OPA_REDIRECT_URL :
        this.disclosure.consultDisclId ? CONSULTING_REDIRECT_URL :
            (this.disclosure.reviewStatusCode === '1' ? CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL);
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: this.disclosure.travelDisclosureId || this.disclosure.disclosureId || this.disclosure.opaDisclosureId || this.disclosure.consultDisclId} });
    }

}
