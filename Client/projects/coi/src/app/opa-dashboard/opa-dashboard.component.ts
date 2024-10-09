import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { fadeInOutHeight, listAnimation, topSlideInOut, slideInAnimation, scaleOutAnimation } from '../common/utilities/animations';
import { NameObject, OPADashboardRequest, OpaDashboardService, SortCountObj } from './opa-dashboard.service';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { Router } from '@angular/router';
import { getEndPointOptionsForLeadUnit } from '../../../../fibi/src/app/common/services/end-point.config';
import { CommonService } from '../common/services/common.service';
import { deepCloneObject, isEmptyObject } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, OPA_DISCLOSURE_ADMIN_RIGHTS, OPA_DISCLOSURE_RIGHTS, OPA_REDIRECT_URL } from '../app-constants';
import { getPersonLeadUnitDetails } from '../common/utilities/custom-utilities';
import { ElasticConfigService } from '../common/services/elastic-config.service';
import { compareDatesWithoutTimeZone, getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../common/utilities/date-utilities';

@Component({
    selector: 'app-opa-dashboard',
    templateUrl: './opa-dashboard.component.html',
    styleUrls: ['./opa-dashboard.component.scss'],
    animations: [fadeInOutHeight, listAnimation, topSlideInOut,
        slideInAnimation('0', '12px', 400, 'slideUp'),
        slideInAnimation('0', '-12px', 400, 'slideDown'),
        scaleOutAnimation('-2px', '0', 200, 'scaleOut'),
    ]
})
export class OpaDashboardComponent implements OnInit {

    isShowAdminDashboard = false;
    localOPARequestObject: OPADashboardRequest = new OPADashboardRequest();
    localNameObject: any;
    sortCountObj: SortCountObj;
    $subscriptions: Subscription[] = [];
    $opaList = new Subject();
    isLoading = false;
    opaList = [];
    isHover: [] = [];
    isViewAdvanceSearch = true;
    result: any = { disclosureCount: 0 };
    advSearchClearField: string;
    leadUnitSearchOptions: any;
    assigneeClearField: String;
    advanceSearchDates = { submissionDate: null, periodStartDate: null, periodEndDate: null };
    localSearchDefaultValues: NameObject = new NameObject();
    lookupValues = [];
    datePlaceHolder = DATE_PLACEHOLDER;
    isShowOPAList = false;
    addAdmin: any = {};
    isAssignAdminModalOpen = false;
    assignAdminPath = 'OPA_DISCLOSURES';
    personElasticOptions: any = {};
    map = new Map();
    sortSectionsList = [
        { variableName: 'person', fieldName: 'Person' },
        { variableName: 'homeUnitName', fieldName: 'Department' },
        { variableName: 'submissionTimestamp', fieldName: 'Certification Date' },
        { variableName: 'closeDate', fieldName: 'Expiration Date' },
        { variableName: 'dispositionStatus', fieldName: 'Disposition Status' },
        { variableName: 'disclosureStatus', fieldName: 'Review Status' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' }
    ];
    opaDisclosureStatusOptions = 'OPA_REVIEW_STATUS_TYPE#REVIEW_STATUS_CODE#true#true';
    opaDispositionStatusOption = 'OPA_DISPOSITION_STATUS_TYPE#DISPOSITION_STATUS_CODE#true#true';
    opaEmployeeRoleTypeOption = 'EMPTY#EMPTY#true#true';
    isShowOptions = false;
    isOPAReviewer = false;

    @ViewChild('mainHeaders', { static: true }) mainHeaders: ElementRef;

    constructor(public _opaDashboardService: OpaDashboardService,
        private _elasticConfigService: ElasticConfigService,
        private _router: Router, public commonService: CommonService, private _navigationService: NavigationService
    ) {  document.addEventListener('mouseup', this.offClickMainHeaderHandler.bind(this)); }

    async ngOnInit() {
        this.isShowAdminDashboard = this.commonService.getAvailableRight(OPA_DISCLOSURE_ADMIN_RIGHTS) || this.commonService.getAvailableRight(OPA_DISCLOSURE_RIGHTS);
        this.isOPAReviewer = this.commonService.isOPAReviewer;
        this.setDashboardTab();
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.getOPADashboard();
        this.checkForSort();
        this.checkForPagination();
        this.checkForAdvanceSearch();
    }

    getPersonLeadUnitDetails(coi: any): string {
        const UNIT_DATA = { unitNumber: '', unitName: ''};
        UNIT_DATA.unitNumber = coi.homeUnit ? coi.homeUnit : '';
        UNIT_DATA.unitName = coi.homeUnitName ? coi.homeUnitName : '';
        return getPersonLeadUnitDetails(UNIT_DATA);
    }

    checkForAdvanceSearch() {
        if (this.isAdvancedSearchMade() && this.checkForPreviousURL()) {
            this.isShowOPAList = true;
            if (this._opaDashboardService.isAdvanceSearch) {
                this.isViewAdvanceSearch = true;
                this.fetchLocalObjectFromServiceObject();
                this.generateLookupArrayForDropdown();
                this.setDefaultSearchOptions();
            } else {
                if (this._opaDashboardService.opaRequestObject.tabType === 'ALL_DISCLOSURES') {
                    this.isViewAdvanceSearch = true;
                    this.isShowOPAList = false;
                } else {
                    this.isViewAdvanceSearch = false;
                }
            }
            this.$opaList.next();
        } else {
            this.resetAdvanceSearchFields();
            if (this._opaDashboardService.opaRequestObject.tabType !== 'ALL_DISCLOSURES') {
                this.$opaList.next();
            }
        }
    }

    generateLookupArrayForDropdown() {
        if (this._opaDashboardService.opaRequestObject.dispositionStatusCodes.length) {
            this.generateLookupArray(this._opaDashboardService.opaRequestObject.dispositionStatusCodes, 'dispositionStatusCodes');
        }
        if (this._opaDashboardService.opaRequestObject.reviewStatusCodes.length) {
            this.generateLookupArray(this._opaDashboardService.opaRequestObject.reviewStatusCodes, 'reviewStatusCodes');
        }
        if (this._opaDashboardService.opaRequestObject.designationStatusCodes.length) {
            this.generateLookupArray(this._opaDashboardService.opaRequestObject.designationStatusCodes, 'designationStatusCodes');
        }
    }

    personSelect(event: any): void {
        this.localOPARequestObject.personId = event ? event.prncpl_id : null;
        this.localSearchDefaultValues.personName = event ? event.full_name : null;
    }

    setDefaultSearchOptions() {
        this.leadUnitSearchOptions.defaultValue = this._opaDashboardService.searchDefaultValues.departmentName || '';
        this.personElasticOptions.defaultValue = this._opaDashboardService.searchDefaultValues.personName || '';
    }

    generateLookupArray(property, propertyNumber) {
        this.lookupValues[propertyNumber] = [];
        property.forEach(element => {
            this.lookupValues[propertyNumber].push({ code: element });
        });
    }

    fetchLocalObjectFromServiceObject() {
        this.localOPARequestObject.unitNumber = this._opaDashboardService.opaRequestObject.unitNumber ?
            this._opaDashboardService.opaRequestObject.unitNumber : null;
        this.localOPARequestObject.personId = this._opaDashboardService.opaRequestObject.personId ?
            this._opaDashboardService.opaRequestObject.personId : null;
        this.localOPARequestObject.dispositionStatusCodes = this._opaDashboardService.opaRequestObject.dispositionStatusCodes ?
            this._opaDashboardService.opaRequestObject.dispositionStatusCodes : [];
        this.localOPARequestObject.reviewStatusCodes = this._opaDashboardService.opaRequestObject.reviewStatusCodes ?
            this._opaDashboardService.opaRequestObject.reviewStatusCodes : [];
        this.localOPARequestObject.designationStatusCodes = this._opaDashboardService.opaRequestObject.designationStatusCodes ?
            this._opaDashboardService.opaRequestObject.designationStatusCodes : [];
        this.advanceSearchDates.submissionDate = this.localOPARequestObject.submissionTimestamp =
            this._opaDashboardService.opaRequestObject.submissionTimestamp ?
                getDateObjectFromTimeStamp(this._opaDashboardService.opaRequestObject.submissionTimestamp) : null;
        this.advanceSearchDates.periodStartDate = this.localOPARequestObject.periodStartDate =
             this._opaDashboardService.opaRequestObject.periodStartDate ?
                    getDateObjectFromTimeStamp(this._opaDashboardService.opaRequestObject.periodStartDate) : null;
        this.advanceSearchDates.periodEndDate = this.localOPARequestObject.periodEndDate =
            this._opaDashboardService.opaRequestObject.periodEndDate ?
                    getDateObjectFromTimeStamp(this._opaDashboardService.opaRequestObject.periodEndDate) : null;
        this.localSearchDefaultValues = this._opaDashboardService.searchDefaultValues;
    }

    isAdvancedSearchMade() {
        return !!Object.values(this._opaDashboardService.opaRequestObject)
            .find(V => V && ((typeof (V) === 'string' && V) || (typeof (V) === 'object' && V.length)));
    }

    setDashboardTab() {
        this._opaDashboardService.opaRequestObject.tabType = sessionStorage.getItem('currentOPATab') ?
            sessionStorage.getItem('currentOPATab') : this.isShowAdminDashboard ? 'ALL_DISCLOSURES' : 'MY_REVIEWS';
    }

    setAdvanceSearch() {
        sessionStorage.setItem('currentOPATab', this._opaDashboardService.opaRequestObject.tabType);
        if (this._opaDashboardService.opaRequestObject.tabType === 'ALL_DISCLOSURES') {
            this.isViewAdvanceSearch = true;
        } else {
            this.isShowOPAList = true;
            this.isViewAdvanceSearch = false;
        }
    }

    checkForPagination() {
        if (this.checkForPreviousURL()) {
            this.localOPARequestObject.currentPage = this._opaDashboardService.opaRequestObject.currentPage;
        }
    }

    checkForSort() {
        if (!isEmptyObject(this._opaDashboardService.opaRequestObject.sort) && this.checkForPreviousURL()) {
            this.localOPARequestObject.sort = deepCloneObject(this._opaDashboardService.opaRequestObject.sort);
            this.sortCountObj = deepCloneObject(this._opaDashboardService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    resetSortObjects() {
        this.localOPARequestObject.sort = { 'updateTimeStamp': 'desc' };
        this._opaDashboardService.opaRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.sortCountObj = new SortCountObj();
        this._opaDashboardService.sortCountObject = new SortCountObj();
    }

    checkForPreviousURL() {
        return ['coi/opa'].some((url) => this._navigationService.previousURL.includes(url));
    }

    getOPADashboard() {
        this.$subscriptions.push(this.$opaList.pipe(
            switchMap(() => {
                this.isLoading = true;
                return this._opaDashboardService.getOPADashboard(this.getRequestObject())
            }))
            .subscribe((data: any) => {
                if (data) {
                    this.result = data || [];
                    this.opaList = data.data;
                }
                this.isLoading = false;
            }, _err => {
                this.isLoading = false;
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
             }));
    }

    sortResult(sortFieldBy) {
        this.sortCountObj[sortFieldBy]++;
        if (this.sortCountObj[sortFieldBy] < 3) {
            this.localOPARequestObject.sort[sortFieldBy] = !this.localOPARequestObject.sort[sortFieldBy] ? 'asc' : 'desc';
        } else {
            this.sortCountObj[sortFieldBy] = 0;
            delete this.localOPARequestObject.sort[sortFieldBy];
        }
        this._opaDashboardService.sortCountObject = deepCloneObject(this.sortCountObj);
        this._opaDashboardService.opaRequestObject.sort = deepCloneObject(this.localOPARequestObject.sort);
        this._opaDashboardService.sort = deepCloneObject(this.localOPARequestObject.sort);
        this.$opaList.next();
    }

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        if (!this.isViewAdvanceSearch) {
            this._opaDashboardService.isAdvanceSearch = false;
        }
    }

    private setSearchOptions() {
        this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit('', this.commonService.fibiUrl);
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
    }

    resetAndPerformAdvanceSearch() {
        this.resetAdvanceSearchFields();
        this.opaList = [];
        this.$opaList.next();
    }

    private resetAdvanceSearchFields() {
        this.resetSortObjects();
        this._opaDashboardService.opaRequestObject.tabType = sessionStorage.getItem('currentOPATab');
        this.localOPARequestObject = new OPADashboardRequest(this._opaDashboardService.opaRequestObject.tabType);
        this.localSearchDefaultValues = new NameObject();
        this._opaDashboardService.searchDefaultValues = new NameObject();
        this._opaDashboardService.opaRequestObject = new OPADashboardRequest(this._opaDashboardService.opaRequestObject.tabType);
        this.advanceSearchDates = { submissionDate: null , periodStartDate: null, periodEndDate: null };
        if (this._opaDashboardService.opaRequestObject.tabType !== 'ALL_DISCLOSURES') {
            this._opaDashboardService.isAdvanceSearch = false;
        }
        this.lookupValues = [];
        this.map.clear();
        this.setSearchOptions();
    }

    performAdvanceSearch() {
        if (this.dateValidation()) {
            this.localOPARequestObject.currentPage = 1;
            this.setAdvanceSearchToServiceObject();
            this.isShowOPAList = true;
            this.opaList = [];
            this._opaDashboardService.isAdvanceSearch = true;
            this.$opaList.next();
        }
    }

    setAdvanceSearchToServiceObject() {
        this._opaDashboardService.opaRequestObject.dispositionStatusCodes = this.localOPARequestObject.dispositionStatusCodes || [];
        this._opaDashboardService.opaRequestObject.reviewStatusCodes = this.localOPARequestObject.reviewStatusCodes || [];
        this._opaDashboardService.opaRequestObject.designationStatusCodes = this.localOPARequestObject.designationStatusCodes || [];
        this._opaDashboardService.opaRequestObject.unitNumber = this.localOPARequestObject.unitNumber || null;
        this._opaDashboardService.opaRequestObject.personId = this.localOPARequestObject.personId || null;
        this._opaDashboardService.opaRequestObject.submissionTimestamp = parseDateWithoutTimestamp(
            this.advanceSearchDates.submissionDate) || null;
        this._opaDashboardService.opaRequestObject.periodStartDate = parseDateWithoutTimestamp(
                this.advanceSearchDates.periodStartDate) || null
        this._opaDashboardService.opaRequestObject.periodEndDate = parseDateWithoutTimestamp(
                    this.advanceSearchDates.periodEndDate) || null;
        this._opaDashboardService.opaRequestObject.currentPage = this.localOPARequestObject.currentPage;
        this._opaDashboardService.searchDefaultValues.departmentName = this.localSearchDefaultValues.departmentName || null;
        this._opaDashboardService.searchDefaultValues.personName = this.localSearchDefaultValues.personName || null;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this.localOPARequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    redirectToDisclosure(opa) {
        this._router.navigate([OPA_REDIRECT_URL],
            { queryParams: { disclosureId: opa.opaDisclosureId } });
    }

    getRequestObject() {
        this.localOPARequestObject.submissionTimestamp = parseDateWithoutTimestamp(this.advanceSearchDates.submissionDate);
        this.localOPARequestObject.periodStartDate = parseDateWithoutTimestamp(this.advanceSearchDates.periodStartDate);
        this.localOPARequestObject.periodEndDate = parseDateWithoutTimestamp(this.advanceSearchDates.periodEndDate);
        this.localOPARequestObject.tabType = sessionStorage.getItem('currentOPATab');
        return this.localOPARequestObject;
    }

    leadUnitChangeFunction(unit: any) {
        this.localOPARequestObject.unitNumber = unit ? unit.unitNumber : null;
        this.localSearchDefaultValues.departmentName = unit ? unit.unitName : null;
    }

    changeTab(tabName) {
        this.opaList = [];
        this.isShowOPAList = false;
        this._opaDashboardService.isAdvanceSearch = false;
        this._opaDashboardService.opaRequestObject.tabType = tabName;
        this._opaDashboardService.opaRequestObject.sort = { 'updateTimeStamp': 'desc' };
        sessionStorage.setItem('currentOPATab', tabName);
        this.resetAdvanceSearchFields();
        this.setAdvanceSearch();
        if (tabName !== 'ALL_DISCLOSURES') {
            this.$opaList.next();
        }
        this.localOPARequestObject.tabType = tabName;
    }

    showAssignAdminButton(opa) {
        return this._opaDashboardService.opaRequestObject.tabType === 'NEW_SUBMISSIONS';
    }

    actionsOnPageChange(event) {
        if (this.localOPARequestObject.currentPage != event) {
            this.localOPARequestObject.currentPage = event;
            this._opaDashboardService.opaRequestObject.currentPage = event;
            this.$opaList.next();
        }
    }

    ngOnDestroy(): void {
        document.removeEventListener('mouseup', null);
        subscriptionHandler(this.$subscriptions);
    }

    openAssignAdminModal(opa): void {
        this.addAdmin.disclosureId = opa.opaDisclosureId;
        this.isAssignAdminModalOpen = true;
    }

    closeAssignAdminModal(event): void {
        if (event) {
            this.addAdmin.disclosureId = null;
            this.$opaList.next();
        }
        this.isAssignAdminModalOpen = false;
    }

    dateValidation() {
		this.map.clear();
		if (this.advanceSearchDates.periodStartDate && this.advanceSearchDates.periodEndDate &&
			compareDatesWithoutTimeZone(this.advanceSearchDates.periodStartDate,
				this.advanceSearchDates.periodEndDate) === 1) {
			this.map.set('fromDate', `Please select To date after From date`);
		}
		return this.map.size < 1 ? true : false;
	}

    getReviewerStatus(statusCode) {
        switch (statusCode) {
            case '2': return 'info';
            case '3': return 'success';
            case '1': return 'warning';
            default: return 'danger';
        }
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

}
