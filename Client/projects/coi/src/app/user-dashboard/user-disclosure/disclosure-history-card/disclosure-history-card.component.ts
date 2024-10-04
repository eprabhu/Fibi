import { Component, Input } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { CREATE_TRAVEL_DISCLOSURE_ROUTE_URL, CREATE_DISCLOSURE_ROUTE_URL, POST_CREATE_DISCLOSURE_ROUTE_URL,
         OPA_REDIRECT_URL, CONSULTING_REDIRECT_URL, DISCLOSURE_TYPE } from '../../../app-constants';
import { Router } from '@angular/router';

@Component({
    selector: 'app-disclosure-history-card',
    templateUrl: './disclosure-history-card.component.html',
    styleUrls: ['./disclosure-history-card.component.scss']
})
export class DisclosureHistoryCardComponent {

    @Input() disclosure: any;
    @Input() isLastElement: any;
    readMoreOrLess = [];
    isPurposeReadMore = false;
    DISCLOSURE_TYPE = DISCLOSURE_TYPE;

    constructor(public commonService: CommonService, private _router: Router) { }

    redirectToDisclosure(): void {
        const redirectUrl = this.disclosure.travelDisclosureId ? CREATE_TRAVEL_DISCLOSURE_ROUTE_URL :
        this.disclosure.opaDisclosureId ? OPA_REDIRECT_URL :
        this.disclosure.consultDisclId ? CONSULTING_REDIRECT_URL :
            (this.disclosure.reviewStatusCode === '1' ? CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL);
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: this.disclosure.travelDisclosureId || this.disclosure.disclosureId || this.disclosure.opaDisclosureId || this.disclosure.consultDisclId} });
    }

}
