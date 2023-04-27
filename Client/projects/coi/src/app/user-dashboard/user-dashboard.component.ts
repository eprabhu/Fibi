import {Component, OnDestroy, OnInit} from '@angular/core';
import {UserDashboardService} from "./user-dashboard.service";
import {subscriptionHandler} from "../../../../fibi/src/app/common/utilities/subscription-handler";
import {Router} from "@angular/router";
import {HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from "../../../../fibi/src/app/app-constants";
import {CommonService} from "../common/services/common.service";
import { SfiService } from '../disclosure/sfi/sfi.service';

declare var $: any;

@Component({
    selector: 'app-user-dashboard',
    templateUrl: './user-dashboard.component.html',
    styleUrls: ['./user-dashboard.component.scss'],
    providers: [UserDashboardService]
})
export class UserDashboardComponent implements OnInit, OnDestroy {

    hasFCOI: any;
    hasActiveFCOI = false;
    isModalOpen = false;
    reviseObject: any = {reviseComment: null, disclosureId: null};
    isReadMore = false;
    headerInfoText = `University policy requires that university officers, faculty, and staff and others acting on its 
    behalf avoid ethical, legal, financial, and other conflicts of interest and ensure that their activities and 
    interests do not conflict with their obligations to the University. Disclosure of financial interests enables 
    the University to determine if a financial interest creates a conflict of interest or the appearance of a 
    conflict of interest. The existence of a conflict or the appearance of one does not imply wrongdoing and 
    does not necessarily mean that a researcher may not retain his or her financial interest and undertake the 
    affected research. Often the University can work with the researcher to manage a conflict or the appearance 
    of a conflict so that the research can continue in a way that minimizes the possibility of bias and preserves 
    the objectivity of the research. Proper management depends on full and prompt disclosure. COI provides the ability 
    to disclose and maintain your Significant Financial Interests; identify potential areas of concern related to your
     proposals and awards; and, disclose reimbursed travel (for NIH compliance).`;
    $subscriptions = [];

    constructor(public service: UserDashboardService, private _router: Router, public commonService: CommonService,
                public sfiService: SfiService) {
    }

    ngOnInit(): void {
        this.getActiveDisclosure();
        this.getAllRemaindersList();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getActiveDisclosure() {
        this.$subscriptions.push(this.service.getActiveDisclosure().subscribe((res: any) => {
            this.service.activeDisclosures = res.coiDisclosures || [];
            this.updateFCOIStatuses();
        }))
    }

    updateFCOIStatuses() {
        this.hasFCOI = this.service.activeDisclosures.find(disclosure =>
            disclosure.fcoiTypeCode == '1'
        );
        this.hasActiveFCOI = this.hasFCOI ? this.hasFCOI.dispositionStatusCode == '2' : false;
    }

    clearModal(): void {
        this.reviseObject.reviseComment = null;
        document.getElementById('triggerReviseModal').click();
    }

    openReviseModal() {
        this.reviseObject = {reviseComment: null, disclosureId: null};
        this.reviseObject.disclosureId = this.hasFCOI.disclosureId;
        this.reviseObject.reviseComment = '';
        document.getElementById('triggerReviseModal').click();
    }

    reviseDisclosure() {
        document.getElementById('triggerReviseModal').click();
        this.$subscriptions.push(this.service.reviseDisclosure(this.reviseObject)
            .subscribe((data: any) => {
                    // this.reviseDisclosureObject = data;
                    // this.clearModal();
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'New version of disclosure created.');
                    this._router.navigate(['/coi/disclosure/summary'], {
                        queryParams: {
                            disclosureId: data.coiDisclosure.disclosureId
                        }
                    });
                },
                err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in creating new version. Please try again.');
                }));
    }

    getAllRemaindersList() {
        this.$subscriptions.push(this.service.getAllRemaindersList().subscribe((res: any) => {
            console.log(res);
        }));
    }

}
