import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription, Subject } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { DATE_PLACEHOLDER } from '../../../../fibi/src/app/app-constants';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForLeadUnit, getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../fibi/src/app/common/services/end-point.config';
import {
    deepCloneObject,
    isEmptyObject,
    setFocusToElement
} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';
import { AdminDashboardService, CoiDashboardRequest, NameObject, SortCountObj } from './admin-dashboard.service';
import { ADMIN_DASHBOARD_RIGHTS,
        HTTP_ERROR_STATUS,
        HTTP_SUCCESS_STATUS,
        POST_CREATE_DISCLOSURE_ROUTE_URL,
        POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';

@Component({
    selector: 'app-admin-dashboard',
    templateUrl: './admin-dashboard.component.html',
    styleUrls: ['./admin-dashboard.component.scss']
})
export class AdminDashboardComponent implements OnInit, OnDestroy {

    setFocusToElement = setFocusToElement;
    DEFAULT_DATE_FORMAT = DATE_PLACEHOLDER;
    isShowDisclosureList = false;
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'did'
    };
    isShowCountModal = false;
    isAddAssignModalOpen = false;
    selectedModuleCode: any;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    disclosureType: any;
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
    clearField: String;
    isHover: [] = [];
    isViewAdvanceSearch = true;
    adminData: any;
    fcoiTypeCode: any;
    isShowAdminDashboard = false;
    hasTravelDisclosureRights = false;
    disclosureTypes: any;
    addAdmin: any = {};
    localCOIRequestObject: CoiDashboardRequest = new CoiDashboardRequest();
    localSearchDefaultValues: NameObject = new NameObject();
    isShowNoDataCard = false;

    constructor(public _coiAdminDashboardService: AdminDashboardService,
        private _router: Router,
        private _elasticConfig: ElasticConfigService,
        public commonService: CommonService,
        private _navigationService: NavigationService
    ) { }

    ngOnInit() {
        this.setDashboardTab();
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.getDashboardCounts();
        this.getPermissions();
        this.checkForSort();
        this.checkForAdvanceSearch();
        this.checkTravelDisclosureRights();
    }

      checkForAdvanceSearch() {
        if (this.isAdvancedSearchMade() && this._navigationService.previousURL.includes('coi/disclosure')) {
            this.isShowDisclosureList = true;
            if (this._coiAdminDashboardService.isAdvanceSearch) {
                this.isViewAdvanceSearch = true;
                this.fetchLocalObjectFromServiceObject();
                this.generateLookupArrayForDropdown();
                this.setDefaultSearchOptions();
            } else {
                if (this._coiAdminDashboardService.coiRequestObject.tabName === 'ALL_DISCLOSURES') {
                    this.isViewAdvanceSearch = true;
                    this.isShowDisclosureList = false;
                } else {
                    this.isViewAdvanceSearch = false;
                }
            }
            this.$coiList.next();
        } else {
            this.resetAndPerformAdvanceSearch();
            this.resetSortObjects();
            this.$coiList.next();
        }
    }

    checkForSort() {
        if(!isEmptyObject(this._coiAdminDashboardService.coiRequestObject.sort) && this._navigationService.previousURL.includes('coi/disclosure')) {
            this.localCOIRequestObject.sort = deepCloneObject(this._coiAdminDashboardService.coiRequestObject.sort);
            this.sortCountObj = deepCloneObject(this._coiAdminDashboardService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    resetSortObjects() {
        this.localCOIRequestObject.sort = {'updateTimeStamp': 'desc'};
        this._coiAdminDashboardService.coiRequestObject.sort = {'updateTimeStamp': 'desc'};
        this.sortCountObj = new SortCountObj();
        this._coiAdminDashboardService.sortCountObject = new SortCountObj();
    }

    setDefaultSearchOptions() {
		this.elasticPersonSearchOptions.defaultValue = this._coiAdminDashboardService.searchDefaultValues.personName || '';
		this.entitySearchOptions.defaultValue = this._coiAdminDashboardService.searchDefaultValues.entityName || '';
        this.leadUnitSearchOptions.defaultValue = this._coiAdminDashboardService.searchDefaultValues.departmentName || '';
	}

    generateLookupArrayForDropdown() {
		if (this._coiAdminDashboardService.coiRequestObject.property4.length) {
			this.generateLookupArray(this._coiAdminDashboardService.coiRequestObject.property4, 'property4');
		}
		if (this._coiAdminDashboardService.coiRequestObject.property5.length) {
			this.generateLookupArray(this._coiAdminDashboardService.coiRequestObject.property5, 'property5');
		}
		if (this._coiAdminDashboardService.coiRequestObject.property20.length) {
			this.generateLookupArray(this._coiAdminDashboardService.coiRequestObject.property20, 'property20');
		}
		if (this._coiAdminDashboardService.coiRequestObject.property21.length) {
			this.generateLookupArray(this._coiAdminDashboardService.coiRequestObject.property21, 'property21');
        }
	}

    generateLookupArray(property, propertyNumber) {
		this.lookupValues[propertyNumber] = [];
		property.forEach(element => {
			this.lookupValues[propertyNumber].push({ code: element });
		});
	}

    isAdvancedSearchMade() {
        return !!Object.values(this._coiAdminDashboardService.coiRequestObject).find(V => V && ((typeof (V) === 'string' && V) || (typeof (V) === 'object' && V.length)));
    }


    private setSearchOptions() {
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.entitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
    }

    setAdvanceSearch() {
        sessionStorage.setItem('currentCOIAdminTab', this._coiAdminDashboardService.coiRequestObject.tabName);
        if (this._coiAdminDashboardService.coiRequestObject.tabName === 'ALL_DISCLOSURES') {
            this.isViewAdvanceSearch = true;
        } else {
            this.isShowDisclosureList = true;
            this.isViewAdvanceSearch = false;
        }
    }

    setDashboardTab() {
        this._coiAdminDashboardService.coiRequestObject.tabName = sessionStorage.getItem('currentCOIAdminTab') ?
             sessionStorage.getItem('currentCOIAdminTab') : 'MY_REVIEWS';
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
                });
            }));
    }

    getDashboardDetails() {
        this.isShowNoDataCard = false;
        this.$subscriptions.push(this.$coiList.pipe(
            switchMap(() => this._coiAdminDashboardService.getCOIAdminDashboard(this.getRequestObject())))
            .subscribe((data: any) => {
                this.result = data || [];
                if (this.result) {
                    this.coiList = this.result.disclosureViews || [];
                    this.isShowNoDataCard = true;
                    this.coiList.map(ele => {
                        ele.numberOfProposals = ele.disclosureStatusCode !== 1 ? ele.noOfProposalInActive : ele.noOfProposalInPending;
                        ele.numberOfAwards = ele.disclosureStatusCode !== 1 ? ele.noOfAwardInActive : ele.noOfAwardInPending;
                    });
                }
                this.setEventTypeFlag();
            }));
    }

    setEventTypeFlag() {
        this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode === '2');
    }

    getRequestObject() {     
        this.setAdvanceSearchValuesToServiceObject();
        this.localCOIRequestObject.tabName = sessionStorage.getItem('currentCOIAdminTab'); 
        return this.localCOIRequestObject;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    actionsOnPageChange(event) {
        this.localCOIRequestObject.currentPage = event;
        this.$coiList.next();
    }

    changeTab(tabName) {
        this.isShowNoDataCard = false;
        this.coiList = [];
        this.isShowDisclosureList = false;
        this._coiAdminDashboardService.isAdvanceSearch = false;
        this._coiAdminDashboardService.coiRequestObject.tabName = tabName;
        this._coiAdminDashboardService.coiRequestObject.sort = {'updateTimeStamp': 'desc'};
        sessionStorage.setItem('currentCOIAdminTab', tabName);
        if (this.isAdvanceSearchTab(tabName)) {
            this.resetAdvanceSearchFields();
            this.setAdvanceSearch();
            if (tabName !== 'ALL_DISCLOSURES') {
                this.$coiList.next();
            }
            return;
        }
        this.localCOIRequestObject.tabName = tabName;
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
        this.localCOIRequestObject.property3 = unit ? unit.unitNumber : null;
        this.localSearchDefaultValues.departmentName = unit ? unit.unitName : null;
    }

    selectPersonName(person: any) {
        this.localCOIRequestObject.property2 = person ? person.prncpl_id : null;
        this.localSearchDefaultValues.personName = person ? person.full_name : null;
    }

    setAdvanceSearchValuesToServiceObject() {
        this.localCOIRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.approvalDate);
        this.localCOIRequestObject.property23 = parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate);
        this.localCOIRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate);
        this.localCOIRequestObject.property15 = null;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this.localCOIRequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    resetAndPerformAdvanceSearch() {
        this.resetAdvanceSearchFields();
        this.$coiList.next();
    }

    selectEntityCountry(country: any) {
        this.localCOIRequestObject.property9 = country ? country.countryCode : null;
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
        this.setAdvanceSearchToServiceObject();
        this.localCOIRequestObject.advancedSearch = 'A';
        this.localCOIRequestObject.currentPage = 1;
        this.isShowDisclosureList = true;
        this._coiAdminDashboardService.isAdvanceSearch = true;
        this.$coiList.next();
    }

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        if (!this.isViewAdvanceSearch) {
            this._coiAdminDashboardService.isAdvanceSearch = false;
        }
    }

    searchConflictIdentified() {
        const conflictIdentifiedStatus = { code: '4', description: 'Conflict identified during Review', dataType: null, isChecked: true };
        this.resetAdvanceSearchFields();
        this.localCOIRequestObject.property4 = [conflictIdentifiedStatus.code];
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
            .subscribe((_res: any) => {
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
        if (this.replyComment[index] !== undefined && this.replyComment[index].trim() && !this.isSaving) {
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
        this._coiAdminDashboardService.coiRequestObject.tabName = sessionStorage.getItem('currentCOIAdminTab');
        this.localCOIRequestObject = new CoiDashboardRequest(this._coiAdminDashboardService.coiRequestObject.tabName);
        this.localSearchDefaultValues = new NameObject();
        this._coiAdminDashboardService.searchDefaultValues = new NameObject();
        this._coiAdminDashboardService.coiRequestObject = new CoiDashboardRequest(this._coiAdminDashboardService.coiRequestObject.tabName);
        this.advanceSearchDates = { approvalDate: null, expirationDate: null, certificationDate: null };
        if(this._coiAdminDashboardService.coiRequestObject.tabName !=='ALL_DISCLOSURES') {
            this._coiAdminDashboardService.isAdvanceSearch = false;
        }
        this.lookupValues = [];
        this.setSearchOptions();
    }

    getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType === 1) {
            if (disclosureSequenceStatusCode === 2 || disclosureSequenceStatusCode === 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode === 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType === 3) {
            return 'Proposal';
        }
    }

    sortResult(sortFieldBy) {
        this.sortCountObj[sortFieldBy]++;
        if (this.sortCountObj[sortFieldBy] < 3) {
            this.localCOIRequestObject.sort[sortFieldBy] = !this.localCOIRequestObject.sort[sortFieldBy] ? 'asc' : 'desc';
        } else {
            this.sortCountObj[sortFieldBy] = 0;
            delete this.localCOIRequestObject.sort[sortFieldBy];
        }
        this._coiAdminDashboardService.sortCountObject = deepCloneObject(this.sortCountObj);
        this._coiAdminDashboardService.coiRequestObject.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this._coiAdminDashboardService.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.$coiList.next();
    }

    selectedEvent(event) {
        this.localCOIRequestObject.property8 = event ? event.entityName: null;
        this.localSearchDefaultValues.entityName = event ? event.entityName : null;
    }

    isActive(colName) {
        if (!isEmptyObject(this.localCOIRequestObject.sort) && colName in this.localCOIRequestObject.sort) {
            return true;
        } else {
            return false;
        }
    }

    async getPermissions() {
        const rightsArray = await this.commonService.fetchPermissions();
        this.isShowAdminDashboard = rightsArray.some((right) => ADMIN_DASHBOARD_RIGHTS.has(right));
    }

    async checkTravelDisclosureRights() {
        const rightsArray = await this.commonService.fetchPermissions();
        this.hasTravelDisclosureRights = rightsArray.some((right) =>
            ['MANAGE_TRAVEL_DISCLOSURE', 'VIEW_TRAVEL_DISCLOSURE'].includes(right));
    }

    getColorBadges(disclosure) {
        if (disclosure?.travelDisclosureId) {
            return 'bg-travel-clip';
        }
        switch (disclosure.fcoiTypeCode) {
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
        if (disclosure.travelDisclosureId) {
            return `#${disclosure.travelDisclosureId}: Travel Disclosure By ${disclosure.travellerName}`;
        }
        if (disclosure.fcoiTypeCode == 1) {
            return `#${disclosure.coiDisclosureNumber}: FCOI Disclosure By ${disclosure.disclosurePersonFullName}`;
        } else if (disclosure.fcoiTypeCode == 2 || disclosure.fcoiTypeCode == 3) {
            return `#${disclosure.coiDisclosureNumber}: Project Disclosure By ${disclosure.disclosurePersonFullName}`;
        }
    }

    formatTravellerListValues(travellerTypes: string): string {
        return travellerTypes ? travellerTypes.split(',').map(value => value.trim()).join(', ') : '';
    }

    redirectToDisclosure(coi) {
        const redirectUrl = coi.travelDisclosureId ? POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: coi.travelDisclosureId || coi.coiDisclosureId } });
    }
    fetchLocalObjectFromServiceObject() {
		this.localCOIRequestObject.property1 = this._coiAdminDashboardService.coiRequestObject.property1 ?
			this._coiAdminDashboardService.coiRequestObject.property1 : null;
		this.localCOIRequestObject.property2 = this._coiAdminDashboardService.coiRequestObject.property2 ?
			this._coiAdminDashboardService.coiRequestObject.property2 : null;
		this.localCOIRequestObject.property3 = this._coiAdminDashboardService.coiRequestObject.property3 ?
			this._coiAdminDashboardService.coiRequestObject.property3 : null;
		this.localCOIRequestObject.property4 = this._coiAdminDashboardService.coiRequestObject.property4.length > 0 ?
			this._coiAdminDashboardService.coiRequestObject.property4 : [];
		this.localCOIRequestObject.property5 = this._coiAdminDashboardService.coiRequestObject.property5.length > 0 ?
			this._coiAdminDashboardService.coiRequestObject.property5 : [];
		this.localCOIRequestObject.property8 = this._coiAdminDashboardService.coiRequestObject.property8 ?
			this._coiAdminDashboardService.coiRequestObject.property8 : null;
		this.localCOIRequestObject.property9 = this._coiAdminDashboardService.coiRequestObject.property9 ?
			this._coiAdminDashboardService.coiRequestObject.property9 : null;
		this.localCOIRequestObject.property10 = this._coiAdminDashboardService.coiRequestObject.property10 ?
			this._coiAdminDashboardService.coiRequestObject.property10 : null;
		this.localCOIRequestObject.property11 = this._coiAdminDashboardService.coiRequestObject.property11 ?
			this._coiAdminDashboardService.coiRequestObject.property11 : null;
		this.localCOIRequestObject.property12 = this._coiAdminDashboardService.coiRequestObject.property15 ?
			this._coiAdminDashboardService.coiRequestObject.property12 : null;
		this.localCOIRequestObject.property13 = this._coiAdminDashboardService.coiRequestObject.property13 ?
			this._coiAdminDashboardService.coiRequestObject.property13 : null;
		this.localCOIRequestObject.property14 = this._coiAdminDashboardService.coiRequestObject.property14 ?
			this._coiAdminDashboardService.coiRequestObject.property14 : null;
		this.localCOIRequestObject.property15 = this._coiAdminDashboardService.coiRequestObject.property15 ?
			this._coiAdminDashboardService.coiRequestObject.property15 : null;
		this.localCOIRequestObject.property21 = this._coiAdminDashboardService.coiRequestObject.property21 ?
			this._coiAdminDashboardService.coiRequestObject.property21 : [];
        this.localCOIRequestObject.property22 = this._coiAdminDashboardService.coiRequestObject.property22 ?
            this._coiAdminDashboardService.coiRequestObject.property22 : null;
        this.advanceSearchDates.approvalDate = this.localCOIRequestObject.property6 = this._coiAdminDashboardService.coiRequestObject.property6 ?
            getDateObjectFromTimeStamp(this._coiAdminDashboardService.coiRequestObject.property6) : null;
        this.advanceSearchDates.expirationDate = this.localCOIRequestObject.property7 = this._coiAdminDashboardService.coiRequestObject.property7 ?
            getDateObjectFromTimeStamp(this._coiAdminDashboardService.coiRequestObject.property7) : null;
        this.advanceSearchDates.certificationDate = this.localCOIRequestObject.property23 = this._coiAdminDashboardService.coiRequestObject.property23 ?
            getDateObjectFromTimeStamp(this._coiAdminDashboardService.coiRequestObject.property23) : null;
		this.localCOIRequestObject.property20 = this._coiAdminDashboardService.coiRequestObject.property20 ?
		    this._coiAdminDashboardService.coiRequestObject.property20 : [];
        this.localCOIRequestObject.advancedSearch = 'A';
        this.localSearchDefaultValues = this._coiAdminDashboardService.searchDefaultValues;
	}

    setAdvanceSearchToServiceObject() {
		this._coiAdminDashboardService.coiRequestObject.property1 = this.localCOIRequestObject.property1 || null;
		this._coiAdminDashboardService.coiRequestObject.property2 = this.localCOIRequestObject.property2 || null;
		this._coiAdminDashboardService.coiRequestObject.property3 = this.localCOIRequestObject.property3 || null;
		this._coiAdminDashboardService.coiRequestObject.property4 = this.localCOIRequestObject.property4 || [];
		this._coiAdminDashboardService.coiRequestObject.property5 = this.localCOIRequestObject.property5 || [];
		this._coiAdminDashboardService.coiRequestObject.property8 = this.localCOIRequestObject.property8 || null;
		this._coiAdminDashboardService.coiRequestObject.property9 = this.localCOIRequestObject.property9 || null;
		this._coiAdminDashboardService.coiRequestObject.property10 = this.localCOIRequestObject.property10 || null;
        this._coiAdminDashboardService.coiRequestObject.property11 = this.localCOIRequestObject.property11 || null;
		this._coiAdminDashboardService.coiRequestObject.property12 = this.localCOIRequestObject.property12 || null;
		this._coiAdminDashboardService.coiRequestObject.property13 = this.localCOIRequestObject.property13 || null;
		this._coiAdminDashboardService.coiRequestObject.property14 = this.localCOIRequestObject.property14 || null;
		this._coiAdminDashboardService.coiRequestObject.property15 = this.localCOIRequestObject.property15 || null;
		this._coiAdminDashboardService.coiRequestObject.property20 = this.localCOIRequestObject.property20 || [];
		this._coiAdminDashboardService.coiRequestObject.property21 = this.localCOIRequestObject.property21 || [];
		this._coiAdminDashboardService.coiRequestObject.property22 = this.localCOIRequestObject.property22 || null;
        this._coiAdminDashboardService.coiRequestObject.property23 = parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate) || null;
        this._coiAdminDashboardService.coiRequestObject.property6 = parseDateWithoutTimestamp(this.localCOIRequestObject.property6) || null;
		this._coiAdminDashboardService.coiRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate) || null;
		this._coiAdminDashboardService.searchDefaultValues.personName = this.localSearchDefaultValues.personName || null;
		this._coiAdminDashboardService.searchDefaultValues.entityName = this.localSearchDefaultValues.entityName || null;
        this._coiAdminDashboardService.searchDefaultValues.departmentName = this.localSearchDefaultValues.departmentName || null;
	}

    openAssignAdminModal(coi) {
        this.addAdmin.disclosureId = coi.coiDisclosureId;
        this.isAddAssignModalOpen = true;
    }

    closeAssignAdminModal(event) {
        if (event) {
            this.addAdmin.disclosureId = null;
            this.$coiList.next();
        }
        this.isAddAssignModalOpen = false;
    }
}

