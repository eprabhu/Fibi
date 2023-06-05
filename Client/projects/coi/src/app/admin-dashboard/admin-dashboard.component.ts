import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription, Subject } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { DATE_PLACEHOLDER } from '../../../../fibi/src/app/app-constants';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForLeadUnit, getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../fibi/src/app/common/services/end-point.config';
import {
    deepCloneObject,
    hideModal,
    isEmptyObject,
    setFocusToElement
} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';
import { AdminDashboardService, CoiDashboardRequest, SortCountObj } from './admin-dashboard.service';
import { CompleterOptions } from '../../../../fibi/src/app/service-request/service-request.interface';
import { ADMIN_DASHBOARD_RIGHTS, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';

@Component({
    selector: 'app-admin-dashboard',
    templateUrl: './admin-dashboard.component.html',
    styleUrls: ['./admin-dashboard.component.scss']
})
export class AdminDashboardComponent {

    setFocusToElement = setFocusToElement;
    DEFAULT_DATE_FORMAT = DATE_PLACEHOLDER;
    currentTab = 'ALL';
    isShowAllProposalList = false;
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'did'
    }
    isShowCountModal = false;
    selectedModuleCode: any;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    disclosureType: any
    datePlaceHolder = DATE_PLACEHOLDER;
    advancedSearch = { hasSFI: true };
    conflictStatusOptions = 'coi_disc_det_status#DISC_DET_STATUS_CODE#true#true';
    disclosureStatusOptions = 'COI_CONFLICT_STATUS_TYPE#CONFLICT_STATUS_CODE#true#true';
    disclosureTypeOptions = 'COI_DISCLOSURE_FCOI_TYPE#FCOI_TYPE_CODE#true#true';
    disPositionOptions = 'COI_DISPOSITION_STATUS_TYPE#DISPOSITION_STATUS_CODE#true#true';
    coiReviewStatusOptions = 'COI_REVIEW_STATUS_TYPE#REVIEW_STATUS_CODE#true#true';
    $subscriptions: Subscription[] = [];
    result: any = { disclosureCount: 0 };
    $coiList = new Subject();
    elasticPersonSearchOptions: any = {};
    leadUnitSearchOptions: any = {};
    countrySearchOptions: any = {};
    lookupValues = [];
    advSearchClearField: String;
    coiList = [];
    isActiveDisclosureAvailable: boolean;
    advanceSearchDates = { approvalDate: null, expirationDate: null, certificationDate: null };
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
    personId: any;
    comments: any[] = [];
    replyComment: any[] = [];
    searchText: any;
    entitySearchOptions: any = {};
    sortCountObj: SortCountObj;
    sortMap: any = {};
    clearField: String;
    isHover: [] = [];
    isViewAdvanceSearch = true;
    adminGroupSearchOptions: any = {};
    clearAdminGroupField: any;
    isShowWarningMessage = false;
    warningMessage: any;
    adminSearchOptions: any = {};
    isAssignToMe = false;
    assignAdminMap = new Map();
    addAdmin: any = {}
    adminData: any;
    fcoiTypeCode: any
    adminGroupsCompleterOptions: CompleterOptions = new CompleterOptions();
    isShowAdminDashboard = false;
    hasTravelDisclosureRights = false;
    disclosureTypes: any;

    constructor( 
        private _router: Router,
        private _elasticConfig: ElasticConfigService,
        public commonService: CommonService,
        public _coiAdminDashboardService: AdminDashboardService 
    ) { }

    ngOnInit() {
        this.sortCountObj = new SortCountObj();
        this.sortMap = {};
        this._coiAdminDashboardService.coiRequestObject.tabName = 'MY_REVIEWS';
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.getDashboardCounts();
        this.getAdminDetails();
        this.getPermissions();
        this.checkTravelDisclosureRights();
    }

    private setSearchOptions() {
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.entitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
    }

    setAdvanceSearch() {
        this.isShowAllProposalList = true;
        if (this._coiAdminDashboardService.coiRequestObject.tabName === 'ALL_DISCLOSURES') {
            this.isViewAdvanceSearch = true;
        } else {
            this.isShowAllProposalList = true;
            this.isViewAdvanceSearch = false;
        }
    }

    toggleADSearch() {
        if (document.getElementById('collapseExample').classList.contains('show')) {
            document.getElementById('collapseExample').classList.remove('show');
        } else {
            document.getElementById('collapseExample').classList.add('show');
        }
    }

    getDashboardCounts() {
        this.$subscriptions.push(this._coiAdminDashboardService.loadDisclosureAdminDashboardCounts()
            .subscribe((res: any) => {
                this.dashboardCounts = res;
                setTimeout(() => {
                    this.getDashboardDetails();
                    this.$coiList.next();
                })
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
            this.setAdvanceSearch();
            if (tabName != 'ALL_DISCLOSURES') {
                this.$coiList.next();
            }
            return;
        }
        this._coiAdminDashboardService.coiRequestObject.tabName = tabName;
    }

    isAdvanceSearchTab(tabName) {
        return ['ALL_DISCLOSURES', 'NEW_SUBMISSIONS', 'NEW_SUBMISSIONS_WITHOUT_SFI', 'MY_REVIEWS', 'ALL_REVIEWS', 'TRAVEL_DISCLOSURES'].includes(tabName);
    }

    fetchMentionedComments() {
        this.$subscriptions.push(this._coiAdminDashboardService.loadCoiReviewComments({
            personId: this.commonService.getCurrentUserDetail('personId'),
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
        this._coiAdminDashboardService.coiRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.approvalDate);
        this._coiAdminDashboardService.coiRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate);
        this._coiAdminDashboardService.coiRequestObject.property23 = parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate);
        this._coiAdminDashboardService.coiRequestObject.property15 = null;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this._coiAdminDashboardService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    resetAndPerformAdvanceSearch() {
        this.resetAdvanceSearchFields();
    }

    selectEntityCountry(country: any) {
        this._coiAdminDashboardService.coiRequestObject.property9 = country ? country.countryCode : null;
    }

    closeModal(event) {
        this.isShowCountModal = event;
    }

    setSelectedModuleCode(moduleName, coi, count = null) {
        if (count > 0) {
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
            this.adminData = coi;
            this.fcoiTypeCode = coi?.fcoiTypeCode;
            this.isShowCountModal = true;
            this.currentDisclosureId = coi.coiDisclosureId;
            this.currentDisclosureNumber = coi.coiDisclosureNumber;
            this.disclosureType = moduleName;
            this.inputType = 'DISCLOSURE_TAB';
            this.personId = coi.personId;
        }
    }
    
    performAdvanceSearch() {
        this._coiAdminDashboardService.coiRequestObject.advancedSearch = 'A';
        this._coiAdminDashboardService.coiRequestObject.currentPage = 1;
        this.isShowAllProposalList = true;
        this.$coiList.next();
    }

    searchConflictIdentified() {
        const conflictIdentifiedStatus = { code: '4', description: 'Conflict identified during Review', dataType: null, isChecked: true };
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
            .subscribe( (_res: any) => {
                // this._router.navigate(['fibi/coi/summary'], { queryParams: { disclosureId: this.selectedStartReviewCoiId }});
                // this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
                this.finishReviewRequest();
                this.$coiList.next();
            }, _err => {
                this.finishReviewRequest();
                // this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    startDisclosureReview() {
        this.$subscriptions.push(this._coiAdminDashboardService
            .startCOIReview(this.selectedStartReviewCoiId)
            .subscribe(async (res: any) => {
                await this._router.navigate(['fibi/coi/summary'], { queryParams: { disclosureId: res.coiDisclosure.disclosureId } });
                this.finishReviewRequest();
            }, _err => {
                this.finishReviewRequest();
                // this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
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
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            case '4': return 'success';
            default: return 'danger';
        }
    }

    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2':
            case '4':
            case '5':
                return 'info';
            case '3': case '6': return 'success';
            default: return 'danger';
        }
    }

    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case 2: return 'success';
            case 3: return 'danger';
            default: return 'info';
        }
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
                    // this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment added successfully.');
                    this.isSaving = false;
                }, _err => {
                    // this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding comment. Please try again.');
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
        this.sortCountObj = new SortCountObj();
        this.sortMap = {};
        this._coiAdminDashboardService.coiRequestObject = new CoiDashboardRequest(this._coiAdminDashboardService.coiRequestObject.tabName);
        this.advanceSearchDates = { approvalDate: null, expirationDate: null, certificationDate: null };
        this.lookupValues = [];
        this.setSearchOptions();
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

    sortResult(sortFieldBy) {
        this.sortCountObj[sortFieldBy]++;
        if (this.sortCountObj[sortFieldBy] < 3) {
            this.sortMap[sortFieldBy] = !this.sortMap[sortFieldBy] ? 'asc' : 'desc';
        } else {
            this.sortCountObj[sortFieldBy] = 0;
            delete this.sortMap[sortFieldBy];
        }
        this._coiAdminDashboardService.coiRequestObject.sort = deepCloneObject(this.sortMap);
        this.$coiList.next();
    }

    selectedEvent(event) {
        this._coiAdminDashboardService.coiRequestObject.property8 = event ? event.coiEntityId : null;
    }

    isActive(colName) {
        if (!isEmptyObject(this._coiAdminDashboardService.coiRequestObject.sort) && colName in this._coiAdminDashboardService.coiRequestObject.sort) {
            return true;
        } else {
            return false;
        }
    }

    assignAdministrator() {
        if (this.validateAdmin()) {
            this.$subscriptions.push(this._coiAdminDashboardService.assignAdmin(this.addAdmin).subscribe(
                (data: any) => {
                    this.isAssignToMe = false;
                    this.isShowWarningMessage = false;
                    this.addAdmin = {};
                    hideModal('assign-to-admin-modal');
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Administrator assigned successfully.');
                    this.$coiList.next();
                }, err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in assigning Administrator.');
                }));
        }
    }

    adminSelectFunction(event: any) {
        if (event) {
            this.getAdminGroupDetails(event.personId);
            this.addAdmin.adminPersonId = event.personId;
            this.isAssignToMe = this.setAssignToMeCheckBox();
            this.assignAdminMap.clear();
        } else {
            this.addAdmin.adminGroupId = null;
            this.addAdmin.adminPersonId = null;
            this.clearAdminGroupField = new String('true');
            this.isAssignToMe = false;
            this.isShowWarningMessage = false;
        }
    }

    getAdminDetails() {
        this.$subscriptions.push(this._coiAdminDashboardService.getAdminDetails().subscribe((data: any) => {
            this.setAdminGroupOptions(data);
            this.setCompleterOptions(this.adminSearchOptions, data.persons, 'fullName');
        }));
    }

    getAdminGroupDetails(personId) {
        this.$subscriptions.push(this._coiAdminDashboardService.getPersonGroup(personId).subscribe((data: any) => {
            if (data.adminGroupId) {
                this.clearAdminGroupField = new String('false');
                this.addAdmin.adminGroupId = data.adminGroupId;
                this.isShowWarningMessage = false;
            } else {
                this.isShowWarningMessage = true;
                this.warningMessage = data;
            }
        }));
    }

    setCompleterOptions(searchOption: any = null, arrayList: any, searchShowField: string) {
        searchOption.defaultValue = '';
        searchOption.arrayList = arrayList || [];
        searchOption.contextField = searchShowField;
        searchOption.filterFields = searchShowField;
        searchOption.formatString = searchShowField;
    }

    setAssignToMeCheckBox() {
        return this.addAdmin.adminPersonId === this.commonService.getCurrentUserDetail('personID') ? true : false;
    }

    assignToMeEvent(checkBoxEvent: any) {
        if (checkBoxEvent.target.checked) {
            this.adminSearchOptions.defaultValue = this.commonService.getCurrentUserDetail('fullName');
            this.clearField = new String('false');
            this.addAdmin.adminPersonId = this.commonService.getCurrentUserDetail('personID');
            this.getAdminGroupDetails(this.commonService.getCurrentUserDetail('personID'));
            this.isAssignToMe = true;
            this.assignAdminMap.clear();
        } else {
            this.clearField = new String('true');
            this.clearAdminGroupField = new String('true');
            this.addAdmin.adminPersonId = null;
            this.isAssignToMe = false;
        }
    }

    adminGroupSelectFunction(event) {
        if (event) {
            this.isShowWarningMessage = false;
            this.addAdmin.adminGroupId = event.adminGroupId;
        } else {
            this.addAdmin.adminGroupId = null;
        }
    }

    validateAdmin() {
        this.assignAdminMap.clear();
        if (!this.addAdmin.adminPersonId) {
            this.assignAdminMap.set('adminName', 'adminName');
        }
        return this.assignAdminMap.size > 0 ? false : true;
    }

    private setAdminGroupOptions(data): void {
        this.adminGroupsCompleterOptions = {
            arrayList: this.getActiveAdminGroups(data),
            contextField: 'adminGroupName',
            filterFields: 'adminGroupName',
            formatString: 'adminGroupName',
            defaultValue: ''
        };
    }

    private getActiveAdminGroups(data) {
        return data.adminGroups.filter(element => element.isActive === 'Y');
    }

    clearAssignFields(id) {
        this.clearField = new String('true');
        this.addAdmin.disclosureId = id;
        this.clearAdminGroupField = new String('true');
        this.addAdmin.adminPersonId = null;
        this.addAdmin.adminGroupId = null;
        this.assignAdminMap.clear();
        this.isShowWarningMessage = false;
    }

    async getPermissions() {
        const rightsArray = await this.commonService.fetchPermissions();
        this.isShowAdminDashboard = rightsArray.some((right) => ADMIN_DASHBOARD_RIGHTS.has(right));
    }

    async checkTravelDisclosureRights() {
        const rightsArray = await this.commonService.fetchPermissions();
        this.hasTravelDisclosureRights = rightsArray.some((right) => ['MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE'].includes(right));
    }

    getColorBadges(moduleName) {
        switch (moduleName) {
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

    modalHeader(disclosure) {
        if (disclosure.fcoiTypeCode == 1) {
            return `#${disclosure.coiDisclosureNumber}: FCOI Disclosure By ${disclosure.disclosurePersonFullName}`;
        } else if (disclosure.fcoiTypeCode == 2 || disclosure.fcoiTypeCode == 3) {
            return `#${disclosure.coiDisclosureNumber}: Project Disclosure By ${disclosure.disclosurePersonFullName}`;
        }
    }

}

