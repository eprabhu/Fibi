import {Component, ElementRef, EventEmitter, Input, OnInit, Output} from '@angular/core';
import { Subscription } from 'rxjs';

import { environment } from '../../../../../environments/environment';
import { CommonService } from '../../../../common/services/common.service';
import { CommentConfiguration, RO } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CoiSummaryService } from '../../coi-summary.service';
import { HTTP_ERROR_STATUS } from '../../../../../../../fibi/src/app/app-constants';
import { DataStoreService } from '../../../services/data-store.service';
import { CoiService } from '../../../services/coi.service';
import { coiReviewComment } from '../../../../shared-components/shared-interface';

declare var $: any;

@Component({
    selector: 'app-relationship-summary',
    templateUrl: './relationship-summary.component.html',
    styleUrls: ['./relationship-summary.component.scss']
})
export class RelationshipSummaryComponent implements OnInit {

    @Input() selectedProject: any;
    @Output() openModuleDetails: EventEmitter<any> = new EventEmitter<any>();
    $subscriptions: Subscription[] = [];
    projectRelations: any = [];
    isOpenSlider = false;
    isShowHoverWhite = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    count: number;

    reviewerConflict: any = { comment: {comments: ''}};
    projectDetails: any = {};
    conflictIndex = -1;
    coiDetails: any = {};
    conflictHistory: any = [];
    isCollapsed = true;
    entityDetails: any = null;
    isReadMore: boolean[] = [];
    projectConflictValidationMap = new Map();
    readMoreOrLess = false;
    resultObject = [];
    showSlider = false;
    entityId: any;
    isDesc = true;
    worstCaseStatus = null;

    constructor(
        private _coiSummaryService: CoiSummaryService,
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        public _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,
        private elementRef: ElementRef
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getEntityProjectRelations();
        this.listenForCoiDetails();
        this.getSfiDetails();
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 3;
    }

    private fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    setSubSectionList() {
        this.commentConfiguration.subSectionList = this.projectRelations.map(ele => {
            return {
                coiSubSectionsCode: ele.disclosureDetailsId,
                description: ele.coiFinancialEntity.coiEntity.coiEntityName,
                coiDisclosureDetails: ele
            };
        });
    }

    private listenForCoiDetails() {
        this.$subscriptions.push(this._dataStoreAndEventsService.dataEvent.subscribe((data: any) => {
            if (data) {
                // this.selectedProject = data;
                this.fetchCOIDetails();
            }
        }));
    }

getEntityProjectRelations() {
        this.$subscriptions.push(
            this._coiSummaryService.getEntityProjectRelations(this.selectedProject.moduleCode, this.selectedProject.moduleItemId,
               Number(this.coiDetails.disclosureId), this.coiDetails.disclosureStatusCode, this.coiDetails.personId)
                .subscribe((data: any) => {
                if (data && data.length > 0) {
                    this.projectRelations = data;
                    this.sortConflictStatus(false);
                    this.setWorstCaseStatus();
                    this.conflictStatusCountUpdation();
                    this.selectedProject.disclosureStatusCount = this.resultObject;
                    }
                }, _err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching project details. Please try again.');
                }));
}

    closePage(event) {
        this.isOpenSlider = false;
        this.entityDetails = null;
        this.getEntityProjectRelations();
        if (event) {
            this.updateDisclosureConflictStatus(event);
        }
        this. conflictStatusCountUpdation();
    }

    setEntityDetails(entity, index) {
        this.entityDetails = {
            disclosureDetailsId: entity.disclosureDetailsId,
            entityName: entity.personEntity?.coiEntity?.entityName,
            index: index,
            personId: this.coiDetails.personId,
            moduleItemKey: this.selectedProject.moduleItemKey,
            moduleItemId: this.selectedProject.moduleItemId,
            moduleCode: this.selectedProject.moduleCode,
            title: this.selectedProject.title,
            coiProjConflictStatusType: entity.coiProjConflictStatusType,
            disclosureId: this.coiDetails.disclosureId,
            comment:entity.disclComment.comment,
            sponsor: this.selectedProject.sponsor,
            primeSponsor: this.selectedProject.primeSponsor
        }
    }

    private updateDisclosureConflictStatus(status): void {
        this.coiDetails.coiConflictStatusType = status;
        this.coiDetails.conflictStatusCode = status.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'], { coiDisclosure: this.coiDetails });
    }

    openReviewerComment(details,section, childSubSection) {
            let coiData = this._dataStore.getData();
            const disclosureDetails:coiReviewComment = {
                documentOwnerPersonId: coiData.coiDisclosure.person.personId,
                disclosureId: coiData.coiDisclosure.disclosureId,
                coiSectionsTypeCode: '6',
                headerName: section === 'PROJECT' ? details.title : details.coiEntity?.entityName,
                coiSubSectionsId: 'PROJECT' ? details.moduleItemId : details.moduleItemKey,
                componentSubRefId: childSubSection?.personEntityId,
                coiSubSectionsTitle: `#${details.moduleCode == '3' ? details.moduleItemId : details.moduleItemKey}: ${details.title}`
            }
            this._commonService.$commentConfigurationDetails.next(disclosureDetails);
            this._coiService.isShowCommentNavBar = true;
    }

    getDisclosureCount(typeCode, disclosureStatus) {
        if(disclosureStatus) {
          let value = disclosureStatus.find(ele => Object.keys(ele) == typeCode);
          return value ? value[typeCode] : 0;
        }
      }

      getSfiDetails() {
        this.$subscriptions.push(this._coiSummaryService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
            }
        }));
    }

    getRequestObject() {
        const REQ_OBJ = new RO();
        REQ_OBJ.currentPage = 0;
        REQ_OBJ.disclosureId = this.coiDetails.disclosureId;
        REQ_OBJ.filterType = 'ACTIVE';
        REQ_OBJ.pageNumber = 0;
        REQ_OBJ.personId = this.coiDetails.personId;
        REQ_OBJ.reviewStatusCode = this.coiDetails.reviewStatusCode;
        REQ_OBJ.searchWord = '';
        return REQ_OBJ;
      }

    conflictStatusCountUpdation() {
        const projectConflictStatusCodes = this.projectRelations.map(relation => relation.projectConflictStatusCode);
        const counts = {};
        projectConflictStatusCodes.forEach(statusCode => {
            if (statusCode === '100') {
                statusCode = '1';
            } else if (statusCode === '200') {
                statusCode = '2';
            } else if (statusCode === '300') {
                statusCode = '3';
            }
            counts[statusCode] = (counts[statusCode] || 0) + 1;
        });
        this.resultObject = [];
        for (const key in counts) {
            if (Object.hasOwnProperty.call(counts, key)) {
                this.resultObject.push({ [key]: counts[key] });
            }
        }

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

    openModule() {
        this.openModuleDetails.emit(this.selectedProject);
    }

    sortConflictStatus(isAsc: boolean) {
        this.isDesc = !isAsc;
        this.projectRelations.sort((a, b) => isAsc ?
            a.coiProjConflictStatusType.projectConflictStatusCode - b.coiProjConflictStatusType.projectConflictStatusCode :
            b.coiProjConflictStatusType.projectConflictStatusCode - a.coiProjConflictStatusType.projectConflictStatusCode);
    }

    setWorstCaseStatus() {
        if (this.projectRelations.length) {
            this.worstCaseStatus = this.projectRelations[0].coiProjConflictStatusType;
        }
    }
}
