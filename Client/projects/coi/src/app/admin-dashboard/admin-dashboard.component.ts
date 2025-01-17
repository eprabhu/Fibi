import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription, Subject } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { getEndPointOptionsForLeadUnit, getEndPointOptionsForCountry, getEndPointOptionsForSponsor } from '../../../../fibi/src/app/common/services/end-point.config';
import {
    deepCloneObject, hideModal,
    isEmptyObject, openModal,
    setFocusToElement
} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';
import { AdminDashboardService } from './admin-dashboard.service';
import {
    CONSULTING_DISCLOSURE_RIGHTS, PROJECT_OVERVIEW_RIGHTS, TRAVEL_DISCLOSURE_RIGHTS,
    CONSULTING_REDIRECT_URL,DATE_PLACEHOLDER, DISCLOSURE_TYPE, FCOI_PROJECT_DISCLOSURE_RIGHTS, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS,
    POST_CREATE_DISCLOSURE_ROUTE_URL,
    POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';
import { fadeInOutHeight, listAnimation, topSlideInOut, slideInAnimation, scaleOutAnimation, leftSlideInOut, heightAnimation } from '../common/utilities/animations';
import { openCoiSlider } from '../common/utilities/custom-utilities';
import { ElasticConfigService } from '../common/services/elastic-config.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../common/utilities/date-utilities';
import { CoiProjectOverviewRequest } from './admin-dashboard.interface';
import { SortCountObj, CoiDashboardRequest, NameObject, CoiDashboardDisclosures } from './admin-dashboard.interface';
import { COICountModal, COICountModalViewSlider } from '../shared-components/shared-interface';

@Component({
    selector: 'app-admin-dashboard',
    templateUrl: './admin-dashboard.component.html',
    styleUrls: ['./admin-dashboard.component.scss'],
    animations: [fadeInOutHeight, listAnimation, topSlideInOut,leftSlideInOut,
        heightAnimation('0', '*', 300, 'heightAnimation'),
        slideInAnimation('0','12px', 400, 'slideUp'),
        slideInAnimation('0','-12px', 400, 'slideDown'),
        scaleOutAnimation('-2px','0', 200, 'scaleOut'),
    ]
})
export class AdminDashboardComponent implements OnInit, OnDestroy {

    @ViewChild('mainHeaders', { static: true }) mainHeaders: ElementRef;

    setFocusToElement = setFocusToElement;
    isShowDisclosureList = false;
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'did'
    };
    isAssignAdminModalOpen = false;
    datePlaceHolder = DATE_PLACEHOLDER;
    advancedSearch = { hasSFI: true };
    conflictStatusOptions = 'coi_disc_det_status#DISC_DET_STATUS_CODE#true#true';
    disclosureStatusOptions = 'COI_CONFLICT_STATUS_TYPE#CONFLICT_STATUS_CODE#true#true';
    disclosureTypeOptions = 'COI_DISCLOSURE_FCOI_TYPE#FCOI_TYPE_CODE#true#true';
    disPositionOptions = 'COI_DISPOSITION_STATUS_TYPE#DISPOSITION_STATUS_CODE#true#true';
    coiReviewStatusOptions = 'COI_REVIEW_STATUS_TYPE#REVIEW_STATUS_CODE#true#true';
    travelDisclosureStatusOptions = 'coi_travel_disclosure_status#TRAVEL_DISCLOSURE_STATUS_CODE#true#true';
    travelDocumentStatusOptions = 'coi_travel_document_status#DOCUMENT_STATUS_CODE#true#true';
    travelReviewStatusOptions = 'coi_travel_review_status#REVIEW_STATUS_CODE#true#true';
    // reviewStatusOptionsForProjectOverview = 'COI_REVIEW_STATUS_TYPE#REVIEW_STATUS_CODE#true#true';
    proposalStatusOptionsForProjectOverview = 'EMPTY#EMPTY#true#true#true#true';
    $subscriptions: Subscription[] = [];
    result: any = { disclosureCount: 0 };
    $coiList = new Subject();
    elasticPersonSearchOptions: any = {};
    leadUnitSearchOptions: any = {};
    countrySearchOptions: any = {};
    lookupValues = [];
    advSearchClearField: string;
    coiList: CoiDashboardDisclosures[] = [];
    isActiveDisclosureAvailable: boolean;
    advanceSearchDates = { approvalDate: null, expirationDate: null, certificationDate: null };
    advacnceSearchDatesForProjectOverview = {startDate : null, endDate : null}
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
    comments: any[] = [];
    replyComment: any[] = [];
    searchText: any;
    entitySearchOptions: any = {};
    sortCountObj: SortCountObj;
    clearField: string;
    clearAdvSearchFields : string;
    countryClearField: string;
    isHover: boolean[] = [];
    isViewAdvanceSearch = true;
    hasReviewerRight = false;
    hasFCOIDiscosureRights = false;
    hasTravelDisclosureRights = false;
    hasConsultingDisclosureRights = false;
    hasProjectOverviewRights = false;
    disclosureTypes: any;
    addAdmin: any = {};
    localCOIRequestObject: CoiDashboardRequest = new CoiDashboardRequest();
    localSearchDefaultValues: NameObject = new NameObject();
    isLoading = false;
    assignAdminPath: 'DISCLOSURES' | 'TRAVEL_DISCLOSURES' | 'CONSULTING_DISCLOSURES' = 'DISCLOSURES';
    sortSectionsList = [];
    showSlider = false;
    entityId: any;
    disclosureSortSections = [
        { variableName: 'disclosurePersonFullName', fieldName: 'Person' },
        { variableName: 'disclosureCategoryType', fieldName: 'Disclosure Type' },
        { variableName: 'disclosureStatus', fieldName: 'Disclosure Status' },
        { variableName: 'dispositionStatus', fieldName: 'Disposition Status' },
        { variableName: 'reviewStatus', fieldName: 'Review Status' },
        { variableName: 'certificationDate', fieldName: 'Certification Date' },
        { variableName: 'expirationDate', fieldName: 'Expiration Date' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
    ];
    travelDisclosureSortSections = [
        { variableName: 'travellerName', fieldName: 'Person' },
        { variableName: 'travelEntityName', fieldName: 'Entity' },
        { variableName: 'travelDisclosureStatusDescription', fieldName: 'Disclosure Status' },
        { variableName: 'reviewDescription', fieldName: 'Review Status' },
        { variableName: 'certifiedAt', fieldName: 'Certification Date' },
        { variableName: 'travelExpirationDate', fieldName: 'Expiration Date' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
    ];
    consultingFormSortSection = [
        { variableName: 'fullName', fieldName: 'Person' },
        { variableName: 'entityName', fieldName: 'Entity' },
        { variableName: 'dispositionStatusDescription', fieldName: 'Disposition Status' },
        { variableName: 'reviewStatusDescription', fieldName: 'Review Status' },
        { variableName: 'certifiedAt', fieldName: 'Certification Date' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
    ];
    readMoreOrLess = [];
    isReadMore = false;
    selectedDisclosures = [];
    isAllDisclosuresSelected = false;
    isFcoiReadMore = false;
    isPurposeRead = false;
    isShowOptions = false;
    sliderElementId = '';
    $projectOverviewList = new Subject<any>();
    localProjectOverviewRO = new CoiProjectOverviewRequest();
    leadUnitSearchOptionsForProjectOverview: any = {};
    sponsorSearchOptionsForProjectOverview: any = {};
    primeSponsorSearchOptionsForProjectOverview: any = {};
    elasticPersonSearchOptionsForProjectOverview: any = {};
    lookupValuesForProjectOverview = [];
    lookupArrayForProjectStatus: any[] = [];
    coiCountModal = new COICountModal();
    DISCLOSURE_TYPE = DISCLOSURE_TYPE;

    constructor(public coiAdminDashboardService: AdminDashboardService,
                private _router: Router,
                private _elasticConfig: ElasticConfigService,
                public commonService: CommonService,
                private _navigationService: NavigationService
    ) { document.addEventListener('mouseup', this.offClickMainHeaderHandler.bind(this)); }

    async ngOnInit() {
        this.getPermissions();
        this.checkTravelDisclosureRights();
        this.checkForConsultingDisclosureRight();
        this.checkForProjectOverviewRight();
        this.setDashboardTab();
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.getDashboardDetails();
        this.checkForSort();
        this.checkForPagination();
        this.checkForAdvanceSearch();
        this.setSearchOptionsForProjectOverview();
    }

    checkForPreviousURL() {
        return ['coi/disclosure', 'coi/travel-disclosure'].some((url) => this._navigationService.previousURL.includes(url));
    }

    checkForAdvanceSearch() {
        if (this.isAdvancedSearchMade() && this.checkForPreviousURL()) {
            this.isShowDisclosureList = true;
            if (this.coiAdminDashboardService.isAdvanceSearch) {
                this.isViewAdvanceSearch = true;
                this.fetchLocalObjectFromServiceObject();
                this.generateLookupArrayForDropdown();
                this.setDefaultSearchOptions();
            } else {
                if (this.coiAdminDashboardService.coiRequestObject.tabName === 'ALL_DISCLOSURES') {
                    this.isViewAdvanceSearch = true;
                    this.isShowDisclosureList = false;
                } else {
                    this.isViewAdvanceSearch = false;
                }
            }
            this.$coiList.next();
        } else {
            this.resetSortObjects();
            this.resetAdvanceSearchFields();
            if (this.coiAdminDashboardService.coiRequestObject.tabName !== 'ALL_DISCLOSURES') {
                this.$coiList.next();
            }
        }
    }

    checkForSort() {
        if (!isEmptyObject(this.coiAdminDashboardService.coiRequestObject.sort) && this.checkForPreviousURL()) {
            this.localCOIRequestObject.sort = deepCloneObject(this.coiAdminDashboardService.coiRequestObject.sort);
            this.sortCountObj = deepCloneObject(this.coiAdminDashboardService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    checkForPagination() {
        if (this.checkForPreviousURL()) {
            this.localCOIRequestObject.currentPage = this.coiAdminDashboardService.coiRequestObject.currentPage;
        }
    }

    resetSortObjects() {
        this.localCOIRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.coiAdminDashboardService.coiRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.sortCountObj = new SortCountObj();
        this.coiAdminDashboardService.sortCountObject = new SortCountObj();
    }

    setDefaultSearchOptions() {
        this.elasticPersonSearchOptions.defaultValue = this.coiAdminDashboardService.searchDefaultValues.personName || '';
        this.entitySearchOptions.defaultValue = this.coiAdminDashboardService.searchDefaultValues.entityName || '';
        this.leadUnitSearchOptions.defaultValue = this.coiAdminDashboardService.searchDefaultValues.departmentName || '';
        this.countrySearchOptions.defaultValue = this.coiAdminDashboardService.searchDefaultValues.travelCountryName || '';
    }

    generateLookupArrayForDropdown() {
        if (this.coiAdminDashboardService.coiRequestObject.property4.length) {
            this.generateLookupArray(this.coiAdminDashboardService.coiRequestObject.property4, 'property4');
        }
        if (this.coiAdminDashboardService.coiRequestObject.property5.length) {
            this.generateLookupArray(this.coiAdminDashboardService.coiRequestObject.property5, 'property5');
        }
        if (this.coiAdminDashboardService.coiRequestObject.property20.length) {
            this.generateLookupArray(this.coiAdminDashboardService.coiRequestObject.property20, 'property20');
        }
        if (this.coiAdminDashboardService.coiRequestObject.property21.length) {
            this.generateLookupArray(this.coiAdminDashboardService.coiRequestObject.property21, 'property21');
        }
    }

    generateLookupArray(property, propertyNumber) {
        this.lookupValues[propertyNumber] = [];
        property.forEach(element => {
            this.lookupValues[propertyNumber].push({ code: element });
        });
    }

    isAdvancedSearchMade() {
        return !!Object.values(this.coiAdminDashboardService.coiRequestObject)
            .find(V => V && ((typeof (V) === 'string' && V) || (typeof (V) === 'object' && V.length)));
    }


    private setSearchOptions() {
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.entitySearchOptions = this._elasticConfig.getElasticForEntity();
    }

    setAdvanceSearch() {
        sessionStorage.setItem('currentCOIAdminTab', this.coiAdminDashboardService.coiRequestObject.tabName);
        if (this.coiAdminDashboardService.coiRequestObject.tabName === 'ALL_DISCLOSURES') {
            this.isViewAdvanceSearch = true;
        } else {
            this.isShowDisclosureList = true;
            this.isViewAdvanceSearch = false;
        }
    }

    setDashboardTab() {
        this.coiAdminDashboardService.coiRequestObject.tabName = sessionStorage.getItem('currentCOIAdminTab') ?
            sessionStorage.getItem('currentCOIAdminTab') : this.getDefaultDashboard();
            if( this.coiAdminDashboardService.coiRequestObject.tabName == 'PROJECTS'){
                this.getLookupDataForProjectStatus();
            }
        this.checkForTravelDisclosureTabChange(this.coiAdminDashboardService.coiRequestObject.tabName);
    }

    private getDefaultDashboard(): string {
        if (this.hasFCOIDiscosureRights) {
            return 'ALL_DISCLOSURES';
        } else if (this.hasReviewerRight) {
            return 'MY_REVIEWS';
        } else if (this.hasTravelDisclosureRights) {
            return 'TRAVEL_DISCLOSURES';
        } else if (this.hasConsultingDisclosureRights) {
            return 'CONSULTING_DISCLOSURES';
        } else if (this.hasProjectOverviewRights) {
            return 'PROJECTS';
        }
    }

    toggleADSearch() {
        if (document.getElementById('collapseExample').classList.contains('show')) {
            document.getElementById('collapseExample').classList.remove('show');
        } else {
            document.getElementById('collapseExample').classList.add('show');
        }
    }

    getDashboardDetails() {
        this.$subscriptions.push(this.$coiList.pipe(
            switchMap(() => {
                this.clearSelectAllDisclosure();
                this.isLoading = true;
                return this.coiAdminDashboardService.getCOIAdminDashboard(this.getRequestObject())
            }))
            .subscribe((data: any) => {
                this.result = data || [];
                if (this.result) {
                    this.coiList = this.getAdminDashboardList();
                    this.isLoading = false;
                    this.coiList.map(ele => {
                        ele.projectHeader = ele.fcoiTypeCode === DISCLOSURE_TYPE.PROJECT ? `#${ele.projectNumber} - ${ele.projectTitle}` : '';
                    });
                }
                this.setEventTypeFlag();
            }));
    }

    private getAdminDashboardList(): any {
        const disclosureViews = this.result.disclosureViews || [];
        const travelDashboardViews = this.result.travelDashboardViews || [];
        const consultingDisclosureView = this.result.consultingDisclDashboardViews || [];
        return [...disclosureViews, ...travelDashboardViews, ...consultingDisclosureView];
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
        document.removeEventListener('mouseup', null);
        subscriptionHandler(this.$subscriptions);
    }

    actionsOnPageChange(event) {
        if (this.localCOIRequestObject.currentPage != event) {
            this.localCOIRequestObject.currentPage = event;
            this.coiAdminDashboardService.coiRequestObject.currentPage = event;
            this.$coiList.next();
        }
    }

    changeTab(tabName) {
        this.coiList = [];
        this.isShowDisclosureList = false;
        this.coiAdminDashboardService.isAdvanceSearch = false;
        this.coiAdminDashboardService.coiRequestObject.tabName = tabName;
        this.coiAdminDashboardService.coiRequestObject.sort = { 'updateTimeStamp': 'desc' };
        sessionStorage.setItem('currentCOIAdminTab', tabName);
        this.checkForTravelDisclosureTabChange(tabName);
        if (this.isAdvanceSearchTab(tabName)) {
            this.resetAdvanceSearchFields();
            this.setAdvanceSearch();
            if (tabName !== 'ALL_DISCLOSURES' && tabName !== 'PROJECTS') {
                this.$coiList.next();
            }
        }
        if(tabName == 'PROJECTS'){
            this.getLookupDataForProjectStatus();
            this.clearAndExecuteAdvancedSearch();
        }
        this.localCOIRequestObject.tabName = tabName;
    }

    private checkForTravelDisclosureTabChange(tabName: string): void {
        if (!['TRAVEL_DISCLOSURES', 'CONSULTING_DISCLOSURES'].includes(tabName)) {
            this.assignAdminPath = 'DISCLOSURES';
            this.sortSectionsList = this.disclosureSortSections;
        } else if(tabName == 'TRAVEL_DISCLOSURES') {
            this.assignAdminPath = tabName;
            this.sortSectionsList = this.travelDisclosureSortSections;
        } else if(tabName == 'CONSULTING_DISCLOSURES'){
            this.assignAdminPath = tabName;
            this.sortSectionsList = this.consultingFormSortSection;
        }
    }

    isAdvanceSearchTab(tabName): boolean {
        return [ 'ALL_DISCLOSURES', 'NEW_SUBMISSIONS', 'NEW_SUBMISSIONS_WITHOUT_SFI',
                 'MY_REVIEWS', 'ALL_REVIEWS', 'TRAVEL_DISCLOSURES', 'CONSULTING_DISCLOSURES', 'PROJECTS' ].includes(tabName);
    }

    fetchMentionedComments() {
        this.$subscriptions.push(this.coiAdminDashboardService.loadCoiReviewComments({
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
        this.coiList = [];
        this.$coiList.next();
    }

    selectEntityCountry(country: any) {
        this.localCOIRequestObject.property9 = country ? country.countryCode : null;
    }

    openCountModal(coi: CoiDashboardDisclosures, count: number | null = null, moduleCode: number = 0): void {
        if (count > 0) {
            this.coiCountModal = {
                personUnit: coi?.unit,
                moduleCode: moduleCode,
                personId: coi?.personId,
                disclosureType: coi?.fcoiTypeCode === DISCLOSURE_TYPE.PROJECT ? coi?.projectType : coi?.fcoiType,
                inputType: 'DISCLOSURE_TAB',
                fcoiTypeCode: coi?.fcoiTypeCode,
                disclosureId: coi?.coiDisclosureId,
                projectHeader: coi?.projectHeader,
                personFullName: coi?.disclosurePersonFullName,
                isOpenCountModal: true
            };
        }
    }

    formatTravellerTypes(travellerTypes: string): string {
        return travellerTypes ? (travellerTypes.split(',').map(travellerType => travellerType.trim()).join(', ')) : '';
    }

    performAdvanceSearch() {
        this.localCOIRequestObject.currentPage = 1;
        this.setAdvanceSearchToServiceObject();
        this.localCOIRequestObject.advancedSearch = 'A';
        this.isShowDisclosureList = true;
        this.coiList = [];
        this.coiAdminDashboardService.isAdvanceSearch = true;
        this.$coiList.next();
    }

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        if (!this.isViewAdvanceSearch) {
            this.coiAdminDashboardService.isAdvanceSearch = false;
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
        this.$subscriptions.push(this.coiAdminDashboardService
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
        this.$subscriptions.push(this.coiAdminDashboardService
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
            this.$subscriptions.push(this.coiAdminDashboardService
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
        this.coiAdminDashboardService.coiRequestObject.tabName = sessionStorage.getItem('currentCOIAdminTab');
        this.localCOIRequestObject = new CoiDashboardRequest(this.coiAdminDashboardService.coiRequestObject.tabName);
        this.localSearchDefaultValues = new NameObject();
        this.coiAdminDashboardService.searchDefaultValues = new NameObject();
        this.coiAdminDashboardService.coiRequestObject = new CoiDashboardRequest(this.coiAdminDashboardService.coiRequestObject.tabName);
        this.advanceSearchDates = { approvalDate: null, expirationDate: null, certificationDate: null };
        if (this.coiAdminDashboardService.coiRequestObject.tabName !== 'ALL_DISCLOSURES') {
            this.coiAdminDashboardService.isAdvanceSearch = false;
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
        this.coiAdminDashboardService.sortCountObject = deepCloneObject(this.sortCountObj);
        this.coiAdminDashboardService.coiRequestObject.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.coiAdminDashboardService.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.$coiList.next();
    }

    selectedEvent(event) {
        this.localCOIRequestObject.property8 = event ? event.entity_name : null;
        this.localSearchDefaultValues.entityName = event ? event.entity_name : null;
    }

    selectedCountryEvent(event) {
        this.localCOIRequestObject.property9 = event ? event.countryName : null;
        this.localSearchDefaultValues.travelCountryName = event ? event.countryName : null;
    }

    isActive(colName) {
        if (!isEmptyObject(this.localCOIRequestObject.sort) && colName in this.localCOIRequestObject.sort) {
            return true;
        } else {
            return false;
        }
    }

    getPermissions() {
        this.hasFCOIDiscosureRights = this.commonService.getAvailableRight(FCOI_PROJECT_DISCLOSURE_RIGHTS);
        this.hasReviewerRight = this.commonService.isCoiReviewer;
    }

    checkTravelDisclosureRights() {
        this.hasTravelDisclosureRights = this.commonService.getAvailableRight(TRAVEL_DISCLOSURE_RIGHTS);
    }

    checkForConsultingDisclosureRight() {
        this.hasConsultingDisclosureRights = this.commonService.getAvailableRight(CONSULTING_DISCLOSURE_RIGHTS);
    }

    async checkForProjectOverviewRight() {
        this.hasProjectOverviewRights = this.commonService.getAvailableRight(PROJECT_OVERVIEW_RIGHTS);
    }

    formatTravellerListValues(travellerTypes: string): string {
        return travellerTypes ? travellerTypes.split(',').map(value => value.trim()).join(', ') : '';
    }

    redirectToDisclosure(coi) {
        const redirectUrl = coi.travelDisclosureId ? POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL : coi.disclosureId ? CONSULTING_REDIRECT_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
        this._router.navigate([redirectUrl],
            { queryParams: { disclosureId: coi.travelDisclosureId || coi.coiDisclosureId || coi.disclosureId } });
    }
    fetchLocalObjectFromServiceObject() {
        this.localCOIRequestObject.property1 = this.coiAdminDashboardService.coiRequestObject.property1 ?
            this.coiAdminDashboardService.coiRequestObject.property1 : null;
        this.localCOIRequestObject.property2 = this.coiAdminDashboardService.coiRequestObject.property2 ?
            this.coiAdminDashboardService.coiRequestObject.property2 : null;
        this.localCOIRequestObject.property3 = this.coiAdminDashboardService.coiRequestObject.property3 ?
            this.coiAdminDashboardService.coiRequestObject.property3 : null;
        this.localCOIRequestObject.property4 = this.coiAdminDashboardService.coiRequestObject.property4.length > 0 ?
            this.coiAdminDashboardService.coiRequestObject.property4 : [];
        this.localCOIRequestObject.property5 = this.coiAdminDashboardService.coiRequestObject.property5.length > 0 ?
            this.coiAdminDashboardService.coiRequestObject.property5 : [];
        this.localCOIRequestObject.property8 = this.coiAdminDashboardService.coiRequestObject.property8 ?
            this.coiAdminDashboardService.coiRequestObject.property8 : null;
        this.localCOIRequestObject.property9 = this.coiAdminDashboardService.coiRequestObject.property9 ?
            this.coiAdminDashboardService.coiRequestObject.property9 : null;
        this.localCOIRequestObject.property10 = this.coiAdminDashboardService.coiRequestObject.property10 ?
            this.coiAdminDashboardService.coiRequestObject.property10 : null;
        this.localCOIRequestObject.property11 = this.coiAdminDashboardService.coiRequestObject.property11 ?
            this.coiAdminDashboardService.coiRequestObject.property11 : null;
        this.localCOIRequestObject.property12 = this.coiAdminDashboardService.coiRequestObject.property15 ?
            this.coiAdminDashboardService.coiRequestObject.property12 : null;
        this.localCOIRequestObject.property13 = this.coiAdminDashboardService.coiRequestObject.property13 ?
            this.coiAdminDashboardService.coiRequestObject.property13 : null;
        this.localCOIRequestObject.property14 = this.coiAdminDashboardService.coiRequestObject.property14 ?
            this.coiAdminDashboardService.coiRequestObject.property14 : null;
        this.localCOIRequestObject.property15 = this.coiAdminDashboardService.coiRequestObject.property15 ?
            this.coiAdminDashboardService.coiRequestObject.property15 : null;
        this.localCOIRequestObject.property21 = this.coiAdminDashboardService.coiRequestObject.property21 ?
            this.coiAdminDashboardService.coiRequestObject.property21 : [];
        this.localCOIRequestObject.property22 = this.coiAdminDashboardService.coiRequestObject.property22 ?
            this.coiAdminDashboardService.coiRequestObject.property22 : null;
        this.advanceSearchDates.approvalDate = this.localCOIRequestObject.property6 =
            this.coiAdminDashboardService.coiRequestObject.property6 ?
            getDateObjectFromTimeStamp(this.coiAdminDashboardService.coiRequestObject.property6) : null;
        this.advanceSearchDates.expirationDate = this.localCOIRequestObject.property7 =
            this.coiAdminDashboardService.coiRequestObject.property7 ?
            getDateObjectFromTimeStamp(this.coiAdminDashboardService.coiRequestObject.property7) : null;
        this.advanceSearchDates.certificationDate = this.localCOIRequestObject.property23 =
            this.coiAdminDashboardService.coiRequestObject.property23 ?
            getDateObjectFromTimeStamp(this.coiAdminDashboardService.coiRequestObject.property23) : null;
        this.localCOIRequestObject.property20 = this.coiAdminDashboardService.coiRequestObject.property20 ?
            this.coiAdminDashboardService.coiRequestObject.property20 : [];
        this.localCOIRequestObject.advancedSearch = 'A';
        this.localSearchDefaultValues = this.coiAdminDashboardService.searchDefaultValues;
    }

    setAdvanceSearchToServiceObject() {
        this.coiAdminDashboardService.coiRequestObject.property1 = this.localCOIRequestObject.property1 || null;
        this.coiAdminDashboardService.coiRequestObject.property2 = this.localCOIRequestObject.property2 || null;
        this.coiAdminDashboardService.coiRequestObject.property3 = this.localCOIRequestObject.property3 || null;
        this.coiAdminDashboardService.coiRequestObject.property4 = this.localCOIRequestObject.property4 || [];
        this.coiAdminDashboardService.coiRequestObject.property5 = this.localCOIRequestObject.property5 || [];
        this.coiAdminDashboardService.coiRequestObject.property8 = this.localCOIRequestObject.property8 || null;
        this.coiAdminDashboardService.coiRequestObject.property9 = this.localCOIRequestObject.property9 || null;
        this.coiAdminDashboardService.coiRequestObject.property10 = this.localCOIRequestObject.property10 || null;
        this.coiAdminDashboardService.coiRequestObject.property11 = this.localCOIRequestObject.property11 || null;
        this.coiAdminDashboardService.coiRequestObject.property12 = this.localCOIRequestObject.property12 || null;
        this.coiAdminDashboardService.coiRequestObject.property13 = this.localCOIRequestObject.property13 || null;
        this.coiAdminDashboardService.coiRequestObject.property14 = this.localCOIRequestObject.property14 || null;
        this.coiAdminDashboardService.coiRequestObject.property15 = this.localCOIRequestObject.property15 || null;
        this.coiAdminDashboardService.coiRequestObject.property20 = this.localCOIRequestObject.property20 || [];
        this.coiAdminDashboardService.coiRequestObject.property21 = this.localCOIRequestObject.property21 || [];
        this.coiAdminDashboardService.coiRequestObject.property22 = this.localCOIRequestObject.property22 || null;
        this.coiAdminDashboardService.coiRequestObject.property23 = parseDateWithoutTimestamp(
            this.advanceSearchDates.certificationDate) || null;
        this.coiAdminDashboardService.coiRequestObject.property6 = parseDateWithoutTimestamp(
            this.localCOIRequestObject.property6) || null;
        this.coiAdminDashboardService.coiRequestObject.property7 = parseDateWithoutTimestamp(
            this.advanceSearchDates.expirationDate) || null;
        this.coiAdminDashboardService.coiRequestObject.currentPage = this.localCOIRequestObject.currentPage;
        this.coiAdminDashboardService.searchDefaultValues.personName = this.localSearchDefaultValues.personName || null;
        this.coiAdminDashboardService.searchDefaultValues.entityName = this.localSearchDefaultValues.entityName || null;
        this.coiAdminDashboardService.searchDefaultValues.departmentName = this.localSearchDefaultValues.departmentName || null;
        this.coiAdminDashboardService.searchDefaultValues.travelCountryName = this.localSearchDefaultValues.travelCountryName || null;
    }

    showAssignAdminButton(coi: any): boolean {
        const tabName = this.coiAdminDashboardService.coiRequestObject.tabName;
        const IS_TAB_FOR_NEW_SUBMISSION = ['NEW_SUBMISSIONS', 'NEW_SUBMISSIONS_WITHOUT_SFI'].includes(tabName);
        const IS_TRAVEL_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_TRAVEL_DISCLOSURE');
        const IS_CONSULTING_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_CONSULTING_DISCLOSURE');
        if (tabName === 'TRAVEL_DISCLOSURES' && coi.reviewStatusCode === '2' && IS_TRAVEL_ADMINISTRATOR) {
            return true;
        } else if (tabName === 'CONSULTING_DISCLOSURES' && coi.reviewStatusCode === '2' && IS_CONSULTING_ADMINISTRATOR) {
            return true;
        } else if (IS_TAB_FOR_NEW_SUBMISSION && this.getManageDisclosureRight(coi.fcoiTypeCode)) {
            return true;
        } else {
            return false;
        }
    }

    getManageDisclosureRight(fcoiTypeCode: string): boolean {
        const IS_FCOI_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_FCOI_DISCLOSURE');
        const IS_PROJECT_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_PROJECT_DISCLOSURE');
        switch (fcoiTypeCode) {
            case DISCLOSURE_TYPE.INITIAL:
            case DISCLOSURE_TYPE.REVISION:
                return IS_FCOI_ADMINISTRATOR;
            case DISCLOSURE_TYPE.PROJECT:
                return IS_PROJECT_ADMINISTRATOR;
            default:
                return ;
        }
    }

    openAssignAdminModal(coi): void {
        this.addAdmin.disclosureId = coi.coiDisclosureId || coi.travelDisclosureId || coi.disclosureId;
        this.isAssignAdminModalOpen = true;
    }

    closeAssignAdminModal(event): void {
        if (event) {
            this.addAdmin.disclosureId = null;
            this.$coiList.next();
        }
        this.isAssignAdminModalOpen = false;
    }

    getReviewerStatus(statusCode) {
        switch (statusCode) {
            case '3': return 'info';
            case '2': return 'success';
            case '1': return 'warning';
            default: return 'danger';
        }
    }

    viewSlider(event: COICountModalViewSlider): void {
        this.showSlider = event.isOpenSlider;
        this.entityId = event.entityId;
        this.sliderElementId = `admin-dashboard-entity-slider-${this.entityId}`;
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.entityId = null;
            this.sliderElementId = '';
	    }, 500);
	}

    checkIfAllDisclosuresSelected() {
        this.isAllDisclosuresSelected = this.selectedDisclosures.filter(index => index).length == this.coiList.length;
    }

    selectAllDisclosures() {
        this.isAllDisclosuresSelected = !this.isAllDisclosuresSelected;
        this.coiList.forEach((_, index) => this.selectedDisclosures[index] = this.isAllDisclosuresSelected);
    }

    atLeastOneIsSelected() {
        return this.selectedDisclosures.filter(index => index).length > 0;
    }

    confirmCompleteReview() {
        openModal('complete-review-confirmation-modal');
    }

    performCompleteReview() {
        this.$subscriptions.push(this.coiAdminDashboardService
            .completeDisclosureReviews({disclosureIdNumberMap: this.generateNumberMap()})
            .subscribe((res) => {
                hideModal('complete-review-confirmation-modal');
                this.$coiList.next();
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Review Completed successfully.');
        }, err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')));
    }

    generateNumberMap() {
        const RO = {};
        this.selectedDisclosures.forEach((selected, index) => {
            if (selected) {
                RO[this.coiList[index].coiDisclosureId.toString()] = this.coiList[index].coiDisclosureNumber;
            }
        });
        return RO;
    }

    // The function is used for closing nav dropdown at mobile screen
    offClickMainHeaderHandler(event: any) {
        if (window.innerWidth < 1200) {
            const ELEMENT = <HTMLInputElement>document.getElementById('navbarResponsive');
            if (document.getElementById('navbarResponsive').classList.contains('show')) {
                document.getElementById('navbarResponsive').classList.remove('show');
            }
        } else {
            this.isShowOptions = false;
        }
    }

    clearSelectAllDisclosure() {
        this.isAllDisclosuresSelected = false;
        this.selectedDisclosures = [];
    }

    selectedItemCount() {
       return this.selectedDisclosures.filter(e => e).length;
    }

    projectTabAdvSearch(): void {
        this.setAdvanceSearchDateValuesToRO();
        this.localProjectOverviewRO.advancedSearch = 'A';
        this.$projectOverviewList.next(this.localProjectOverviewRO);
        this.setAdvanceSearchOfProjectOverviewToServiceObject();
    }

    setAdvanceSearchOfProjectOverviewToServiceObject(): void {
        this.coiAdminDashboardService.projectOverviewRequestObject.property2 = this.localProjectOverviewRO.property2  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property3 = this.localProjectOverviewRO.property3  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property6 = this.localProjectOverviewRO.property6  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property4 = this.localProjectOverviewRO.property4  || [];
        this.coiAdminDashboardService.projectOverviewRequestObject.property5 = this.localProjectOverviewRO.property5  || [];
        this.coiAdminDashboardService.projectOverviewRequestObject.property9 = this.localProjectOverviewRO.property9  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property11 = this.localProjectOverviewRO.property11  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.personId = this.localProjectOverviewRO.personId  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property13 =  parseDateWithoutTimestamp(this.localProjectOverviewRO.property13)  || null;
        this.coiAdminDashboardService.projectOverviewRequestObject.property14 =  parseDateWithoutTimestamp(this.localProjectOverviewRO.property14)  || null;
    }

    private setSearchOptionsForProjectOverview(): void {
        this.leadUnitSearchOptionsForProjectOverview = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
        this.sponsorSearchOptionsForProjectOverview = getEndPointOptionsForSponsor({ baseUrl: this.commonService.fibiUrl });
        this.primeSponsorSearchOptionsForProjectOverview = getEndPointOptionsForSponsor({ baseUrl: this.commonService.fibiUrl });
        this.elasticPersonSearchOptionsForProjectOverview = this._elasticConfig.getElasticForPerson();
    }

    leadUnitUpdateFunction(unit: any): void {
        this.localProjectOverviewRO.property3 = unit ? unit.unitNumber : null;
    }

    sponsorUpdateFunction(sponsor : any): void {
            this.localProjectOverviewRO.property9 = sponsor ? sponsor.sponsorCode : null;
    }

    primeSponsorUpdateFunction(primeSponosr:any): void {
        this.localProjectOverviewRO.property11 = primeSponosr ? primeSponosr.sponsorCode : null;
    }

    fetchPersonName(person : any): void {
        this.localProjectOverviewRO.personId = person ? person.prncpl_id : null;
    }

    onLookupSelectData(data: any, property: string): void {
        this.lookupValuesForProjectOverview[property] = data;
        this.localProjectOverviewRO[property] = data.length ? data.map(d => d.code) : [];
    }

    setAdvanceSearchDateValuesToRO(): void {
        this.localProjectOverviewRO.property13 = parseDateWithoutTimestamp(this.advacnceSearchDatesForProjectOverview.startDate);
        this.localProjectOverviewRO.property14 = parseDateWithoutTimestamp(this.advacnceSearchDatesForProjectOverview.endDate);
    }

    clearAndExecuteAdvancedSearch(): void {
        this.localProjectOverviewRO = new CoiProjectOverviewRequest();
        this.$projectOverviewList.next(this.localProjectOverviewRO);
        this.advacnceSearchDatesForProjectOverview = {startDate : null, endDate : null};
        this.lookupValuesForProjectOverview = [];
        this.setSearchOptionsForProjectOverview();
    }

    getLookupDataForProjectStatus(): void {
        this.$subscriptions.push(this.coiAdminDashboardService.getLookupDataForProposalStatus().subscribe((res: any) => {
            this.lookupArrayForProjectStatus = res;
        }))
    }
}
