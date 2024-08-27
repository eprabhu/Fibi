import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { environment } from '../../../../../environments/environment';
import { CoiSummaryEventsAndStoreService } from '../../services/coi-summary-events-and-store.service';
import { CommentConfiguration, RO } from '../../../coi-interface';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { CommonService } from '../../../../common/services/common.service';
import { CoiService } from '../../../services/coi.service';
import { DataStoreService } from '../../../services/data-store.service';
import { coiReviewComment } from '../../../../shared-components/shared-interface';
import { openCoiSlider } from '../../../../common/utilities/custom-utilities';
import { CoiSummaryService } from '../../services/coi-summary.service';
import { HTTP_ERROR_STATUS } from '../../../../app-constants';

@Component({
    selector: 'app-sfi-summary',
    templateUrl: './sfi-summary.component.html',
    styleUrls: ['./sfi-summary.component.scss'],
})
export class SfiSummaryComponent implements OnInit, OnDestroy {

    @ViewChild('viewSFISummaryDetailsOverlay', { static: true }) viewSFISummaryDetailsOverlay: ElementRef;
    coiFinancialEntityDetails: any[] = [];
    $subscriptions: Subscription[] = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    coiDetails: any = {};
    searchText: string;
    showSlider = false;
    entityId: any;
    isLoading = false;
    sliderElementId = ''; 

    constructor(public coiService: CoiService,
                private _dataStore: DataStoreService,
                private _commonService :CommonService,
                private _coiSummaryService: CoiSummaryService,
                private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService
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
        this.isLoading = false;
        this.$subscriptions.push(this._coiSummaryService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.isLoading = true;
                this.coiFinancialEntityDetails = data.personEntities;
                this.setSubSectionList();
            }
        }, (_error: any) => {
            this.isLoading = true;
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching SFI list failed. Please try again.');
        }));
    }

    getRequestObject() {
		const REQ_OBJ = new RO();
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

    viewSlider(event) {
        this.showSlider = event.flag;
        this.entityId = event.entityId;
        this.sliderElementId = `sfi-summary-${this.entityId}`
        openCoiSlider(this.sliderElementId)
    }

    hideSfiNavBar() {      
        setTimeout(() => {
            this.sliderElementId = '';
            this.entityId = null;
            this.showSlider = false;
        },500);
    }

    openReviewerComment(event) {
        let coiData = this._dataStore.getData();
        const REQ_BODY:coiReviewComment = {
            documentOwnerPersonId: coiData.coiDisclosure.disclosureId,
            subModuleItemKey: event.personEntityId,
            componentTypeCode: '5',
            coiSubSectionsTitle: event.personEntityHeader
        }
        this._commonService.$commentConfigurationDetails.next(REQ_BODY);
        this.coiService.isShowCommentNavBar = true;
    }

}
