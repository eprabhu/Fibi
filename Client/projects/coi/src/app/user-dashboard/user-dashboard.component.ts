import { Component, OnDestroy, OnInit } from '@angular/core';
import { UserDashboardService } from './user-dashboard.service';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { Router } from '@angular/router';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { environment } from '../../environments/environment';
import { CommonService } from '../common/services/common.service';

declare const $: any;

@Component({
    selector: 'app-user-dashboard',
    templateUrl: './user-dashboard.component.html',
    styleUrls: ['./user-dashboard.component.scss'],
    providers: [UserDashboardService]
})
export class UserDashboardComponent implements OnInit, OnDestroy {

    deployMap = environment.deployUrl;
    hasFCOI: any;
    isModalOpen = false;
    reviseObject: any = { revisionComment: null, disclosureId: null };
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
    isShowCreateOrReviseModal = false;
    triggeredFrom = '';

    constructor(public service: UserDashboardService, private _router: Router, public commonService: CommonService,
        public sfiService: SfiService, public router: Router) {
    }

    ngOnInit(): void {
        this.getActiveDisclosure();
        this.openModalTriggeredFromChild();
        // this.getAllRemaindersList();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getActiveDisclosure() {
        this.$subscriptions.push(this.service.getActiveDisclosure().subscribe((res: any) => {
            this.service.activeDisclosures = res.coiDisclosures || [];
            this.updateFCOIStatuses();
        }));
    }

    updateFCOIStatuses() {
        this.hasFCOI = this.service.activeDisclosures.find(disclosure =>
            disclosure.fcoiTypeCode === '1'
        );
    }

    openReviseModal() {
        this.reviseObject = {revisionComment: null, disclosureId: null};
        this.reviseObject.revisionComment = '';
        this.triggeredFrom = 'FCOI_DISCLOSURE';
        this.isShowCreateOrReviseModal = true;
    }

    getAllRemaindersList() {
        this.$subscriptions.push(this.service.getAllRemaindersList().subscribe((res: any) => {
        }));
    }

    outputEventAction(event) {
        if (event.closeModal != null) {
            this.isShowCreateOrReviseModal = event.closeModal;
        }
    }

    openProjectDisclosure() {
        this.triggeredFrom = 'PROJECT_DISCLOSURE';
        this.isShowCreateOrReviseModal = true;
    }

    openTravelDisclosure(): void {
        this.triggeredFrom = 'TRAVEL_DISCLOSURE';
        this.isShowCreateOrReviseModal = true;
    }

    openCreateSFI() {
        this._router.navigate(['/coi/create-sfi/create'], { queryParams: { type: 'SFI' } });
    }

    openModalTriggeredFromChild() {
        this.$subscriptions.push(this.service.$openModal.subscribe((event: string) => {
            if(event == 'FCOI') {
                this.openReviseModal();
            }
        }))
    }
}
