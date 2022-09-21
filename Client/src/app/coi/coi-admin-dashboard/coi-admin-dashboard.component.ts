import {Component, OnDestroy, OnInit} from '@angular/core';
import {CoiAdminDashboardService, CoiDashboardRequest} from './coi-admin-dashboard.service';
import {subscriptionHandler} from '../../common/utilities/subscription-handler';
import {Subject, Subscription} from 'rxjs';
import {ElasticConfigService} from '../../common/services/elastic-config.service';
import {switchMap} from 'rxjs/operators';
import {getEndPointOptionsForCountry, getEndPointOptionsForLeadUnit} from '../../common/services/end-point.config';
import {setFocusToElement} from '../../common/utilities/custom-utilities';
import {DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from '../../app-constants';
import {CommonService} from '../../common/services/common.service';
import {parseDateWithoutTimestamp} from '../../common/utilities/date-utilities';
import {Router} from '@angular/router';
import { DataStoreService } from '../services/data-store.service';

@Component({
    selector: 'app-coi-admin-dashboard',
    templateUrl: './coi-admin-dashboard.component.html',
    styleUrls: ['./coi-admin-dashboard.component.css']
})
export class CoiAdminDashboardComponent implements OnInit, OnDestroy {

    setFocusToElement = setFocusToElement;
    DEFAULT_DATE_FORMAT = DEFAULT_DATE_FORMAT;
    currentTab = 'ALL';
    coiElastic = null;
    isShowCountModal = false;
    selectedModuleCode: any;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    advancedSearch = {hasSFI: true};
    disclosureStatusOptions = 'coi_disclosure_status#DISCLOSURE_STATUS_CODE#true#true';
    disclosureTypeOptions = 'coi_disclosure_category_type#DISCLOSURE_CATEGORY_TYPE_CODE#true#true';
    $subscriptions: Subscription[] = [];
    result: any = {disclosureCount: 0};
    $coiList = new Subject();
    elasticPersonSearchOptions: any = {};
    leadUnitSearchOptions: any = {};
    countrySearchOptions: any = {};
    lookupValues = [];
    advSearchClearField: String;
    coiList = [];
    isActiveDisclosureAvailable: boolean;
    advanceSearchDates = {startDate: null, endDate: null};
    selectedStartReviewCoiId = null;
    selectedReviewStatusCode = null;
    isSaving = false;
    dashboardCounts = {
        conflictIdentifiedCount: 0,
        pendingEntityApproval: 0,
        unassignedCount: 0,
        newSubmissionsCount: 0,
        reviewCommentsCount: 0
    };
    inputType: any;
    disclosureSequenceStatusCode: any;
    personId: any;
    comments: any[] = [];
    replyComment: any[] = [];

    constructor(public _coiAdminDashboardService: CoiAdminDashboardService,
                private _router: Router,
                public dataStoreService: DataStoreService,
                private _elasticConfig: ElasticConfigService,
                public commonService: CommonService) {
    }

    ngOnInit() {
        this.coiElastic = this._elasticConfig.getElasticForCoi();
        this.getDashboardDetails();
        this.getDashboardCounts();
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit();
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.$coiList.next();
    }

    getDashboardCounts() {
        this.$subscriptions.push(this._coiAdminDashboardService.loadDisclosureAdminDashboardCounts()
            .subscribe((res: any) => {
                this.dashboardCounts = res;
            }));
    }

    getDashboardDetails() {
        this.$subscriptions.push(this.$coiList.pipe(
            switchMap(() => this._coiAdminDashboardService.getCOIAdminDashboard(this.getRequestObject())))
            .subscribe((data: any) => {
                this.result = data || [];
                if (this.result) {
                    this.coiList = this.result.disclosureViews || [];
                    this.coiList.map(ele => {
                        ele.numberOfProposals = ele.disclosureStatusCode != 1 ? ele.noOfProposalInActive : ele.noOfProposalInPending;
                        ele.numberOfAwards = ele.disclosureStatusCode != 1 ? ele.noOfAwardInActive : ele.noOfAwardInPending;
                    });
                }
                this.setEventTypeFlag();
            }));
    }

    setEventTypeFlag() {
        this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode == '2');
    }

    getRequestObject() {
        this.setAdvanceSearchValuesToServiceObject();
        return this._coiAdminDashboardService.coiRequestObject;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    actionsOnPageChange(event) {
        this._coiAdminDashboardService.coiRequestObject.currentPage = event;
        this.$coiList.next();
    }

    changeTab(tabName) {
        this.coiList = [];
        if (this.isAdvanceSearchTab(tabName)) {
            this.resetAdvanceSearchFields();
            this._coiAdminDashboardService.coiRequestObject.tabName = tabName;
            this.$coiList.next();
            return;
        }
        if (tabName == 'COMMENTS') {
            this.fetchMentionedComments();
        }
        this._coiAdminDashboardService.coiRequestObject.tabName = tabName;
    }

    isAdvanceSearchTab(tabName) {
        return ['ALL_DISCLOSURES', 'PENDING_DISCLOSURES', 'NEW_SUBMISSIONS', 'ENTITY'].includes(tabName);
    }

    fetchMentionedComments() {
        this.$subscriptions.push(this._coiAdminDashboardService.loadCoiReviewComments({
            personId: this.commonService.getCurrentUserDetail('personID'),
            disclosureId: null,
            coiSubSectionsId: null,
            coiSectionsTypeCode: null,
            sort: 'desc'
        }).subscribe((res: any) => {
            this.comments = this.setCommentArray(res.coiReviewComments);
        }));
    }

    setCommentArray(commentArray: any) {
        const COMMENT_ARRAY = [];
        commentArray.forEach(comment => {
            if (!comment.coiParentCommentId) {
                const COMMENT = JSON.parse(JSON.stringify(comment));
                COMMENT.childComments = this.filterChildComments(comment.coiReviewCommentId, commentArray);
                COMMENT.isShowReplies = false;
                COMMENT_ARRAY.push(COMMENT);
            }
        });
        return COMMENT_ARRAY;
    }

    filterChildComments(parentCommentId: string, commentArray: any) {
        return commentArray.filter(comment => comment.coiParentCommentId === parentCommentId);
    }

    leadUnitChangeFunction(unit: any) {
        this._coiAdminDashboardService.coiRequestObject.property3 = unit ? unit.unitNumber : null;
    }

    selectPersonName(person: any) {
        this._coiAdminDashboardService.coiRequestObject.property2 = person ? person.prncpl_id : null;
    }

    setAdvanceSearchValuesToServiceObject() {
        this._coiAdminDashboardService.coiRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
        this._coiAdminDashboardService.coiRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
        this._coiAdminDashboardService.coiRequestObject.property15 =
            this._coiAdminDashboardService.coiRequestObject.advancedSearch === 'L'
                ? null : this._coiAdminDashboardService.coiRequestObject.property15;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this._coiAdminDashboardService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    resetAndPerformAdvanceSearch() {
        this.resetAdvanceSearchFields();
        this.$coiList.next();
    }

    selectEntityCountry(country: any) {
        this._coiAdminDashboardService.coiRequestObject.property9 = country ? country.countryCode : null;
    }

    closeModal(event) {
        this.isShowCountModal = event;
    }

    setSelectedModuleCode(moduleName, id, coiNumber, disSeqCode, personId) {
        switch (moduleName) {
            case 'sfi':
                this.selectedModuleCode = 8;
                break;
            case 'award':
                this.selectedModuleCode = 1;
                break;
            case 'proposal':
                this.selectedModuleCode = 3;
                break;
            default:
                this.selectedModuleCode = 0;
        }
        this.isShowCountModal = true;
        this.currentDisclosureId = id;
        this.currentDisclosureNumber = coiNumber;
        this.inputType = 'DISCLOSURE_TAB';
        this.disclosureSequenceStatusCode = disSeqCode;
        this.personId = personId;
    }

    performAdvanceSearch() {
        this._coiAdminDashboardService.coiRequestObject.advancedSearch = 'A';
        this._coiAdminDashboardService.coiRequestObject.currentPage = 1;
        this.$coiList.next();
    }

    searchConflictIdentified() {
        const conflictIdentifiedStatus = {code: '4', description: 'Conflict identified during Review', dataType: null, isChecked: true};
        this.resetAdvanceSearchFields();
        this._coiAdminDashboardService.coiRequestObject.property4 = [conflictIdentifiedStatus.code];
        this.lookupValues['property4'] = [conflictIdentifiedStatus];
        this.$coiList.next();
    }

    processDisclosureReview() {
        if (!this.isSaving && this.selectedStartReviewCoiId) {
            this.isSaving = true;
            if (this.selectedReviewStatusCode === '1') {
                this.startDisclosureReview();
            } else {
                this.completeDisclosureReview();
            }
        }
    }

    completeDisclosureReview() {
        this.$subscriptions.push(this._coiAdminDashboardService
            .completeCOIReview(this.selectedStartReviewCoiId)
            .subscribe(async (_res: any) => {
                // await this._router.navigate(['fibi/coi/summary'], { queryParams: { disclosureId: this.selectedStartReviewCoiId }});
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
                this.finishReviewRequest();
                this.$coiList.next();
            }, _err => {
                this.finishReviewRequest();
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    startDisclosureReview() {
        this.$subscriptions.push(this._coiAdminDashboardService
            .startCOIReview(this.selectedStartReviewCoiId)
            .subscribe(async (res: any) => {
                await this._router.navigate(['fibi/coi/summary'], {queryParams: {disclosureId: res.coiDisclosure.disclosureId}});
                this.finishReviewRequest();
            }, _err => {
                this.finishReviewRequest();
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    finishReviewRequest() {
        this.isSaving = false;
        this.clearSelectedReview();
    }

    clearSelectedReview() {
        this.selectedStartReviewCoiId = null;
        this.selectedReviewStatusCode = null;
    }

    getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1':
                return 'warning';
            case '2':
                return 'info';
            case '3':
                return 'success';
            default:
                return 'danger';
        }
    }

    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case 1:
                return 'warning';
            case 2:
            case 4:
            case 5:
                return 'info';
            case 3:
            case 6:
                return 'success';
            default:
                return 'danger';
        }
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case 1:
                return 'warning';
            case 2:
            case 3:
                return 'success';
            default:
                return 'info';
        }
    }

    redirectToDisclosure(coi: any) {
        this._router.navigate(['fibi/coi'], {queryParams: {disclosureId: coi.disclosure_id}});
    }

    getSubSectionDescription(comment: any) {
        return comment.coiSectionsTypeCode === '2' ? comment.coiFinancialEntity.coiEntity.coiEntityName :
            comment.coiDisclosureDetails.coiFinancialEntity.coiEntity.coiEntityName;
    }

    performReplyComment(comment: any, index: number) {
        if (this.replyComment[index] != undefined && this.replyComment[index].trim() && !this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._coiAdminDashboardService
                .addCOIReviewComment(this.getCommentRequestObj(comment, index))
                .subscribe((res: any) => {
                    this.replyComment[index] = '';
                    comment.childComments.push(res.coiReviewComment);
                    comment.isShowReplies = true;
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment added successfully.');
                    this.isSaving = false;
                }, _err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding comment. Please try again.');
                    this.isSaving = false;
                }));
        }
    }

    getCommentRequestObj(comment: any, index: number) {
        return {
            coiReviewComment: {
                coiReviewId: comment.coiReviewId,
                disclosureId: comment.disclosureId,
                coiReviewActivityId: comment.coiReviewActivityId,
                coiSectionsTypeCode: comment.coiSectionsTypeCode,
                coiSectionsType: comment.coiSectionsType,
                coiSubSectionsId: comment.coiSubSectionsId,
                coiReviewCommentTag: [],
                comment: this.replyComment[index],
                coiParentCommentId: comment.coiReviewCommentId,
                isPrivate: comment.isPrivate
            }
        };
    }

    private resetAdvanceSearchFields() {
        this._coiAdminDashboardService.coiRequestObject = new CoiDashboardRequest();
        this.advanceSearchDates = {startDate: null, endDate: null};
        this.advSearchClearField = new String('true');
        this.lookupValues = [];
    }

    getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType == 1) {
            if (disclosureSequenceStatusCode == 2 || disclosureSequenceStatusCode == 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType == 3) {
            return 'Proposal';
        }
    }

}
