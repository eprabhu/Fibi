import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { CoiService } from '../../../services/coi.service';
import { CoiSummaryService } from '../../coi-summary.service';
import { environment } from '../../../../../environments/environment';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CommentConfiguration, GetSFIRequestObject } from '../../../coi-interface';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Router } from '@angular/router';


@Component({
    selector: 'app-sfi-summary',
    templateUrl: './sfi-summary.component.html',
    styleUrls: ['./sfi-summary.component.css']
})
export class SfiSummaryComponent implements OnInit, OnDestroy {

    coiFinancialEntityDetails: any[] = [];
    $subscriptions: Subscription[] = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    coiDetails: any = {};
    searchText: string;
    isCollapsed = true;

    constructor(
        private _coiSummaryService: CoiSummaryService,
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _router: Router
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getSfiDetails();
        this.commentConfiguration.coiSectionsTypeCode = 2;
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
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

    getSfiDetails() {
        this.$subscriptions.push(this._coiSummaryService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.coiFinancialEntityDetails = data.personEntities;
                this.setSubSectionList();
            }
        }));
    }

    getRequestObject() {
		const REQ_OBJ = new GetSFIRequestObject();
        REQ_OBJ.currentPage = 0;
        REQ_OBJ.disclosureId = Number(this.coiDetails.disclosureId);
        REQ_OBJ.filterType = '';
        REQ_OBJ.pageNumber = 0;
        REQ_OBJ.personId = this.coiDetails.personId;
        REQ_OBJ.reviewStatusCode = '';
        REQ_OBJ.searchWord = '';
        return REQ_OBJ;
    }

    setSubSectionList() {
        this.commentConfiguration.subSectionList = this.coiFinancialEntityDetails.map(ele => {
            return {
                coiSubSectionsCode: ele.coiFinancialEntityId,
                description: ele.coiEntity.coiEntityName,
                coiEntity: ele.coiEntity
            };
        });
    }

    modifyReviewComment(isSubSectionComment = false, subSectionCode = null) {
        this.commentConfiguration.isSubSectionComment = isSubSectionComment;
        this.commentConfiguration.coiSubSectionsId = subSectionCode;
        this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
    }

}
