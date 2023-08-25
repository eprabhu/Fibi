import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { CoiSummaryService } from '../../coi-summary.service';
import { environment } from '../../../../../environments/environment';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CommentConfiguration, RO } from '../../../coi-interface';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Router } from '@angular/router';
import { CommonService } from '../../../../../app/common/services/common.service';
import { CoiService } from '../../../services/coi.service';
import { DataStoreService } from '../../../services/data-store.service';
import { coiReviewComment } from '../../../../shared-components/shared-interface';


@Component({
    selector: 'app-sfi-summary',
    templateUrl: './sfi-summary.component.html',
    styleUrls: ['./sfi-summary.component.css']
})
export class SfiSummaryComponent implements OnInit, OnDestroy {

    @ViewChild('viewSFISummaryDetailsOverlay', { static: true }) viewSFISummaryDetailsOverlay: ElementRef;
    coiFinancialEntityDetails: any[] = [];
    $subscriptions: Subscription[] = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    coiDetails: any = {};
    searchText: string;
    isCollapsed = true;
    showSlider = false;
    entityId: any;
    isShowNoDataCard = false;

    constructor(
        private _coiSummaryService: CoiSummaryService,
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _router: Router,private _commonService :CommonService,private _coiService: CoiService,private _dataStore: DataStoreService
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
        this.isShowNoDataCard = false;
        this.$subscriptions.push(this._coiSummaryService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.isShowNoDataCard = true;
                this.coiFinancialEntityDetails = data.personEntities;
                this.setSubSectionList();
            }
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
        document.getElementById('COI_SCROLL').classList.add('overflow-hidden');
        setTimeout(() => {
            const slider = document.querySelector('.slider-base');
            slider.classList.add('slider-opened');
        });
    }

    hideSfiNavBar() {
        this.addBodyScroll();
        let slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');        
        setTimeout(() => {
            this.showSlider = false;
        },500);
    }

    addBodyScroll() {
        document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
        document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
    }

    openReviewerComment(event) {
        let coiData = this._dataStore.getData();
        const REQ_BODY:coiReviewComment = {
            documentOwnerPersonId: coiData.coiDisclosure.disclosureId,
            disclosureId: coiData.coiDisclosure.disclosureId,
            coiSectionsTypeCode: '5',
            headerName: event.personEntityHeader,
            coiSubSectionsId: event.personEntityId,
            componentSubRefId: null
        }
        this._commonService.$commentConfigurationDetails.next(REQ_BODY);
        this._coiService.isShowCommentNavBar = true;
    }
}
