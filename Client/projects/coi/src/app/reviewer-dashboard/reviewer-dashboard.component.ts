import { Component, OnInit } from '@angular/core';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForEntity, getEndPointOptionsForLeadUnit } from '../../../../fibi/src/app/common/services/end-point.config';
import { deepCloneObject, isEmptyObject } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { NameObject, ReviewerDashboardRequest, ReviewerDashboardService, SortCountObj } from './reviewer-dashboard.service';
import { CommonService } from '../common/services/common.service';
import { NavigationService } from '../common/services/navigation.service';
import { listAnimation, topSlideInOut, fadeInOutHeight, scaleOutAnimation, slideInAnimation} from '../common/utilities/animations';
import { DATE_PLACEHOLDER } from '../../../src/app/app-constants';

@Component({
    selector: 'app-reviewer-dashboard',
    templateUrl: './reviewer-dashboard.component.html',
    styleUrls: ['./reviewer-dashboard.component.scss'],
    animations: [listAnimation, topSlideInOut, fadeInOutHeight, 
        slideInAnimation('0','12px', 400, 'slideUp'),
        slideInAnimation('0','-12px', 400, 'slideDown'),
        scaleOutAnimation('-2px','0', 200, 'scaleOut'),
    ]
})
export class ReviewerDashboardComponent implements OnInit {

    dashboardCounts = {
        conflictIdentifiedCount: 0,
        pendingEntityApproval: 0
    };
    EntitySearchOptions: any = {};
    elasticPersonSearchOptions: any = {};
    clearField: any;
    lookupValues = [];
    leadUnitSearchOptions: any = {};
    advSearchClearField: String;
    disclosureStatusOptions = 'COI_CONFLICT_STATUS_TYPE#CONFLICT_STATUS_CODE#true#true';
    disclosureTypeOptions = 'COI_DISCLOSURE_FCOI_TYPE#FCOI_TYPE_CODE#true#true';
    advanceSearchDates = { certificationDate: null, expirationDate: null };
    $subscriptions: Subscription[] = [];
    $coiList = new Subject();
    result: any = { disclosureCount: 0 };
    coiList = [];
    isShowAllProposalList = false;
    sortMap: any = {};
    sortCountObj: SortCountObj;
    isActiveDisclosureAvailable: boolean;
    selectedModuleCode: any;
    reviewerData: any;
    disclosureType; any;
    currentDisclosureId: any;
    currentDisclosureNumber: any;
    personId: any;
    fcoiTypeCode: any;
    isShowCountModal = false;
    inputType: any;
    isHover: [] = [];
    isViewAdvanceSearch = true;
    isShowDisclosureList = false;
    localCOIRequestObject: ReviewerDashboardRequest = new ReviewerDashboardRequest();
    localSearchDefaultValues: NameObject = new NameObject();
    isLoading = false;
    readMoreOrLess = [];
    sortSectionsList = [
        { variableName: 'coiDisclosureNumber', fieldName: 'Disclosure#' },
        { variableName: 'disclosurePersonFullName', fieldName: 'Person' },
        { variableName: 'disclosureCategoryType', fieldName: 'Disclosure Type' },
        { variableName: 'disclosureStatus', fieldName: 'Disclosure Status' },
        { variableName: 'certifiedAt', fieldName: 'Certification Date' },
        { variableName: 'expirationDate', fieldName: 'Expiration Date' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
    ];
    datePlaceHolder = DATE_PLACEHOLDER;

    constructor(
        public reviewerDashboardService: ReviewerDashboardService,
        public commonService: CommonService,
        private _elasticConfig: ElasticConfigService,
        private _navigationService: NavigationService) { }

    ngOnInit() {
        this.setDashboardTab();
        this.getDashboardDetails();
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.checkForSort();
        this.checkForPagination();
        this.checkForAdvanceSearch();
    }

    actionsOnPageChange(event) {
        this.localCOIRequestObject.currentPage = event;
        this.reviewerDashboardService.reviewerRequestObject.currentPage = event;
        this.$coiList.next();
    }

    checkForPagination() {
        if (this._navigationService.previousURL.includes('coi/disclosure')) {
            this.localCOIRequestObject.currentPage = this.reviewerDashboardService.reviewerRequestObject.currentPage;
        }
    }

    getDashboardDetails() {
        this.$subscriptions.push(this.$coiList.pipe(
            switchMap(() => {
                this.isLoading = true;
                return this.reviewerDashboardService.getCOIReviewerDashboard(this.getRequestObject())
        }))
            .subscribe((data: any) => {
                this.result = data || [];
                this.loadingComplete();
                if (this.result) {
                    this.coiList = this.result.disclosureViews || [];
                    this.coiList.map(ele => {
                        ele.numberOfProposals = ele.disclosureStatusCode !== 1 ? ele.noOfProposalInActive : ele.noOfProposalInPending;
                        ele.numberOfAwards = ele.disclosureStatusCode !== 1 ? ele.noOfAwardInActive : ele.noOfAwardInPending;
                    });
                }
                this.setEventTypeFlag();
            }, (err) => {
                this.loadingComplete();
            }));
    }

    private loadingComplete() {
        this.isLoading = false;
    }

    getRequestObject() {
        this.setAdvanceSearchValuesToServiceObject();
        this.localCOIRequestObject.tabName = sessionStorage.getItem('currentCOIReviewTab');
        return this.localCOIRequestObject;
    }


    setAdvanceSearchValuesToServiceObject() {
        this.localCOIRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate);
        this.localCOIRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate);
    }

    selectPersonName(person: any) {
        this.localCOIRequestObject.property2 = person ? person.prncpl_id : null;
        this.localSearchDefaultValues.personName = person ? person.full_name : null;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this.localCOIRequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    leadUnitChangeFunction(unit: any) {
        this.localCOIRequestObject.property3 = unit ? unit.unitNumber : null;
        this.localSearchDefaultValues.departmentName = unit ? unit.unitName : null;
    }

    resetAndPerformAdvanceSearch() {
        this.resetAdvanceSearchFields();
        this.coiList = [];
        this.$coiList.next();
    }

    selectedEvent(event) {
        this.localCOIRequestObject.property8 = event ? event.coiEntityId : null;
        this.localSearchDefaultValues.entityName = event ? event.entityName : null;
    }

    performAdvanceSearch() {
        this.localCOIRequestObject.currentPage = 1;
        this.setAdvanceSearchToServiceObject();
        this.localCOIRequestObject.advancedSearch = 'A';
        this.isShowDisclosureList = true;
        this.reviewerDashboardService.isAdvanceSearch = true;
        this.coiList = [];
        this.$coiList.next();
    }

    isActive(colName) {
        if (!isEmptyObject(this.localCOIRequestObject.sort) && colName in this.localCOIRequestObject.sort) {
            return true;
        } else {
            return false;
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
        this.reviewerDashboardService.sortCountObject = deepCloneObject(this.sortCountObj);
        this.reviewerDashboardService.reviewerRequestObject.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.reviewerDashboardService.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.$coiList.next();
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

    setEventTypeFlag() {
        this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode === '2');
    }

    setSelectedModuleCode(moduleName, id, coiNumber, personId, count = null, coi) {
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
            this.disclosureType = moduleName;
            this.currentDisclosureId = id;
            this.currentDisclosureNumber = coiNumber;
            this.personId = personId;
            this.reviewerData = coi;
            this.fcoiTypeCode = coi.fcoiTypeCode;
            this.isShowCountModal = true;
            this.inputType = 'DISCLOSURE_TAB';
        }
    }

    closeModalEvent(event) {
        if (!event) {
            this.isShowCountModal = event;
        }
    }

    modalHeader(coi) { if (coi.fcoiTypeCode === '2' || coi.fcoiTypeCode === '3') {
            if (coi.fcoiTypeCode === '2') {
                return `# ${coi.proposalId} - ${coi.proposalTitle}`;
            } else if (coi.fcoiTypeCode === '3') {
                return `# ${coi.awardId} - ${coi.awardTitle}`;
            }
        }
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

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        if (!this.isViewAdvanceSearch) {
            this.reviewerDashboardService.isAdvanceSearch = false;
        }
    }

    setAdvanceSearch() {
        sessionStorage.setItem('currentCOIReviewTab', this.reviewerDashboardService.reviewerRequestObject.tabName);
        if (this.reviewerDashboardService.reviewerRequestObject.tabName === 'HISTORY') {
            this.isViewAdvanceSearch = true;
        } else {
            this.isShowDisclosureList = true;
            this.isViewAdvanceSearch = false;
        }
    }

    isAdvanceSearchTab(tabName) {
        return [
            'NEW_SUBMISSIONS',
            'MY_REVIEWS',
            'HISTORY'
        ].includes(tabName);
    }

    changeTab(tabName) {
        this.coiList = [];
        this.isLoading = true;
        this.isShowDisclosureList = false;
        this.reviewerDashboardService.isAdvanceSearch = false;
        this.reviewerDashboardService.reviewerRequestObject.tabName = tabName;
        this.reviewerDashboardService.reviewerRequestObject.sort = { 'updateTimeStamp': 'desc' };
        sessionStorage.setItem('currentCOIReviewTab', tabName);
        if (this.isAdvanceSearchTab(tabName)) {
            this.resetAdvanceSearchFields();
            this.setAdvanceSearch();
            if (tabName !== 'HISTORY') {
                this.$coiList.next();
            }
            return;
        }
        this.localCOIRequestObject.tabName = tabName;
    }

    private resetAdvanceSearchFields() {
        this.sortCountObj = new SortCountObj();
        this.reviewerDashboardService.reviewerRequestObject.tabName = sessionStorage.getItem('currentCOIReviewTab');
        this.localCOIRequestObject = new ReviewerDashboardRequest(this.reviewerDashboardService.reviewerRequestObject.tabName);
        this.localSearchDefaultValues = new NameObject();
        this.reviewerDashboardService.searchDefaultValues = new NameObject();
        this.advanceSearchDates = { certificationDate: null, expirationDate: null };
        this.reviewerDashboardService.reviewerRequestObject =
            new ReviewerDashboardRequest(this.reviewerDashboardService.reviewerRequestObject.tabName);
        if (this.reviewerDashboardService.reviewerRequestObject.tabName !== 'HISTORY') {
            this.reviewerDashboardService.isAdvanceSearch = false;
        }
        this.lookupValues = [];
        this.setSearchOptions();

    }

    setDashboardTab() {
        this.reviewerDashboardService.reviewerRequestObject.tabName = sessionStorage.getItem('currentCOIReviewTab') ?
            sessionStorage.getItem('currentCOIReviewTab') : 'NEW_SUBMISSIONS';
    }

    checkForSort() {
        if (!isEmptyObject(this.reviewerDashboardService.reviewerRequestObject.sort) && this._navigationService.previousURL.includes('coi/disclosure')) {
            this.localCOIRequestObject.sort = deepCloneObject(this.reviewerDashboardService.reviewerRequestObject.sort);
            this.sortCountObj = deepCloneObject(this.reviewerDashboardService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    isAdvancedSearchMade() {
        return !!Object.values(this.reviewerDashboardService.reviewerRequestObject).find(V => V &&
            ((typeof (V) === 'string' && V) || (typeof (V) === 'object' && V.length)));
    }

    checkForAdvanceSearch() {
        if (this.isAdvancedSearchMade() && this._navigationService.previousURL.includes('coi/disclosure')) {
            this.isShowDisclosureList = true;
            if (this.reviewerDashboardService.isAdvanceSearch) {
                this.isViewAdvanceSearch = true;
                this.fetchLocalObjectFromServiceObject();
                this.generateLookupArrayForDropdown();
                this.setDefaultSearchOptions();
            } else {
                if (this.reviewerDashboardService.reviewerRequestObject.tabName === 'HISTORY') {
                    this.isViewAdvanceSearch = true;
                    this.isShowDisclosureList = false;
                } else {
                    this.isViewAdvanceSearch = false;
                }
            }
            this.$coiList.next();
        } else {
            this.resetSortObjects();
            this.resetAndPerformAdvanceSearch();
        }
    }

    fetchLocalObjectFromServiceObject() {
        // Disclosure #
        this.localCOIRequestObject.property1 = this.reviewerDashboardService.reviewerRequestObject.property1 ?
            this.reviewerDashboardService.reviewerRequestObject.property1 : null;
        // Person
        this.localCOIRequestObject.property2 = this.reviewerDashboardService.reviewerRequestObject.property2 ?
            this.reviewerDashboardService.reviewerRequestObject.property2 : null;
        // Department
        this.localCOIRequestObject.property3 = this.reviewerDashboardService.reviewerRequestObject.property3 ?
            this.reviewerDashboardService.reviewerRequestObject.property3 : null;
        // Disclosure Status
        this.localCOIRequestObject.property4 = this.reviewerDashboardService.reviewerRequestObject.property4 ?
            this.reviewerDashboardService.reviewerRequestObject.property4 : [];
        // Disclosure Type
        this.localCOIRequestObject.property5 = this.reviewerDashboardService.reviewerRequestObject.property5 ?
            this.reviewerDashboardService.reviewerRequestObject.property5 : [];
        // Expiration Date
        this.localCOIRequestObject.property6 = this.reviewerDashboardService.reviewerRequestObject.property6 ?
            getDateObjectFromTimeStamp(this.reviewerDashboardService.reviewerRequestObject.property6) : null;
        // Certification date
        this.localCOIRequestObject.property7 = this.reviewerDashboardService.reviewerRequestObject.property7 ?
            getDateObjectFromTimeStamp(this.reviewerDashboardService.reviewerRequestObject.property7) : null;
        // Entity
        this.localCOIRequestObject.property8 = this.reviewerDashboardService.reviewerRequestObject.property8 ?
            this.reviewerDashboardService.reviewerRequestObject.property8 : null;
        // Unknown
        this.localCOIRequestObject.property9 = this.reviewerDashboardService.reviewerRequestObject.property9 ?
            this.reviewerDashboardService.reviewerRequestObject.property9 : null;
        this.localCOIRequestObject.advancedSearch = 'A';
        this.localSearchDefaultValues = this.reviewerDashboardService.searchDefaultValues;
    }

    generateLookupArrayForDropdown() {
        if (this.reviewerDashboardService.reviewerRequestObject.property4.length) {
            this.generateLookupArray(this.reviewerDashboardService.reviewerRequestObject.property4, 'property4');
        }
        if (this.reviewerDashboardService.reviewerRequestObject.property5.length) {
            this.generateLookupArray(this.reviewerDashboardService.reviewerRequestObject.property5, 'property5');
        }
    }

    generateLookupArray(property, propertyNumber) {
        this.lookupValues[propertyNumber] = [];
        property.forEach(element => {
            this.lookupValues[propertyNumber].push({code: element});
        });
    }

    setDefaultSearchOptions() {
        this.elasticPersonSearchOptions.defaultValue = this.reviewerDashboardService.searchDefaultValues.personName || '';
        this.EntitySearchOptions.defaultValue = this.reviewerDashboardService.searchDefaultValues.entityName || '';
        this.leadUnitSearchOptions.defaultValue = this.reviewerDashboardService.searchDefaultValues.departmentName || '';
    }

    resetSortObjects() {
        this.localCOIRequestObject.sort = {'updateTimeStamp' : 'desc'};
        this.reviewerDashboardService.reviewerRequestObject.sort = {'updateTimeStamp' : 'desc'};
        this.sortCountObj = new SortCountObj();
        this.reviewerDashboardService.sortCountObject = new SortCountObj();
    }

    setAdvanceSearchToServiceObject() {
        this.reviewerDashboardService.reviewerRequestObject.property1 = this.localCOIRequestObject.property1 || null;
        this.reviewerDashboardService.reviewerRequestObject.property2 = this.localCOIRequestObject.property2 || null;
        this.reviewerDashboardService.reviewerRequestObject.property3 = this.localCOIRequestObject.property3 || null;
        this.reviewerDashboardService.reviewerRequestObject.property4 = this.localCOIRequestObject.property4 || [];
        this.reviewerDashboardService.reviewerRequestObject.property5 = this.localCOIRequestObject.property5 || [];
        this.reviewerDashboardService.reviewerRequestObject.property6 =
            parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate) || [];
        this.reviewerDashboardService.reviewerRequestObject.property7 =
            parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate) || [];
        this.reviewerDashboardService.reviewerRequestObject.property8 = this.localCOIRequestObject.property8 || null;
        this.reviewerDashboardService.reviewerRequestObject.currentPage = this.localCOIRequestObject.currentPage;
        this.reviewerDashboardService.searchDefaultValues.personName = this.localSearchDefaultValues.personName || null;
        this.reviewerDashboardService.searchDefaultValues.entityName = this.localSearchDefaultValues.entityName || null;
        this.reviewerDashboardService.searchDefaultValues.departmentName = this.localSearchDefaultValues.departmentName || null;
    }

    private setSearchOptions() {
        this.EntitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl, 'ALL');
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
    }
}
