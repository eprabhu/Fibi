import { Component, OnDestroy, OnInit } from '@angular/core';

import { environment } from '../../../../../environments/environment';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../../common/services/common.service';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { CoiService } from '../../../services/coi.service';
import { DataStoreService } from '../../../services/data-store.service';
import { coiReviewComment } from '../../../../shared-components/shared-interface';

@Component({
    selector: 'app-certify-summary',
    templateUrl: './certify-summary.component.html',
    styleUrls: ['./certify-summary.component.css']
})
export class CertifySummaryComponent implements OnInit,OnDestroy {

    coiDetails: any = {};
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    $subscriptions: Subscription[] = [];


    constructor(
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,private _commonService: CommonService,private coiService: CoiService,private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 4;
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    // modifyReviewComment() {
    //         let coiData = this._dataStore.getData();
    //         const disclosureDetails:coiReviewComment = {
    //             disclosureId: coiData.coiDisclosure.disclosureId,
    //             coiSectionsTypeCode: '7',
    //             documentOwnerPersonId: coiData.coiDisclosure.person.personId,
    //             coiSubSectionsId: null,
    //             headerName: '',
    //             componentSubRefId: null,
    //             coiSubSectionsTitle: null
    //         }
    //         this._commonService.$commentConfigurationDetails.next(disclosureDetails);	
    //     this.coiService.isShowCommentNavBar = true;
    // }

}
