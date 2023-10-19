import { Component, OnInit } from '@angular/core';
import { fadeInOutHeight, listAnimation, topSlideInOut, slideInAnimation, scaleOutAnimation } from '../common/utilities/animations';
import { NameObject, OPADashboardRequest, OpaDashboardService, SortCountObj } from './opa-dashboard.service';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { Router } from '@angular/router';
import { getEndPointOptionsForLeadUnit } from '../../../../fibi/src/app/common/services/end-point.config';
import { CommonService } from '../common/services/common.service';
import { deepCloneObject, isEmptyObject } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { NavigationService } from '../common/services/navigation.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { DATE_PLACEHOLDER } from '../app-constants';

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

    isShowAdminDashboard = true;
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
    advanceSearchDates = { submissionDate: null };
    localSearchDefaultValues: NameObject = new NameObject();
    lookupValues = [];
    datePlaceHolder = DATE_PLACEHOLDER;
    isShowOPAList = false;
    addAdmin: any = {};
    isAssignAdminModalOpen = false;
    assignAdminPath = 'OPA_DISCLOSURES';
    sortSectionsList = [
        { variableName: 'person', fieldName: 'Person' },
        { variableName: 'homeUnitNumber', fieldName: 'Department' },
        { variableName: 'submissionTimestamp', fieldName: 'Certification Date' },
        { variableName: 'closeDate', fieldName: 'Expiration Date' },
        { variableName: 'dispositionStatus', fieldName: 'Disposition Status' },
        { variableName: 'disclosureStatus', fieldName: 'Review Status' },
        { variableName: 'updateTimeStamp', fieldName: 'Last Updated' }
    ];
    opaDisclosureStatusOptions = 'OPA_DISCLOSURE_STATUS_TYPE#OPA_DISCLOSURE_STATUS_CODE#true#true';
    opaDispositionStatusOption = 'OPA_DISPOSITION_STATUS_TYPE#DISPOSITION_STATUS_CODE#true#true';

    constructor(public _opaDashboardService: OpaDashboardService, private _router: Router, public commonService: CommonService, private _navigationService: NavigationService
    ) { }

    ngOnInit() {
        this.setDashboardTab();
        this.setSearchOptions();
        this.setAdvanceSearch();
        this.getOPADashboard();
        this.checkForSort();
        this.checkForPagination();
        this.checkForAdvanceSearch();
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
        if (this._opaDashboardService.opaRequestObject.opaDisclosureStatusCodes.length) {
            this.generateLookupArray(this._opaDashboardService.opaRequestObject.opaDisclosureStatusCodes, 'opaDisclosureStatusCodes');
        }
    }

    setDefaultSearchOptions() {
        this.leadUnitSearchOptions.defaultValue = this._opaDashboardService.searchDefaultValues.departmentName || '';
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
        this.localOPARequestObject.dispositionStatusCodes = this._opaDashboardService.opaRequestObject.dispositionStatusCodes ?
            this._opaDashboardService.opaRequestObject.dispositionStatusCodes : [];
        this.localOPARequestObject.opaDisclosureStatusCodes = this._opaDashboardService.opaRequestObject.opaDisclosureStatusCodes ?
            this._opaDashboardService.opaRequestObject.opaDisclosureStatusCodes : [];
        this.advanceSearchDates.submissionDate = this.localOPARequestObject.submissionTimestamp =
            this._opaDashboardService.opaRequestObject.submissionTimestamp ?
                getDateObjectFromTimeStamp(this._opaDashboardService.opaRequestObject.submissionTimestamp) : null;
        this.localSearchDefaultValues = this._opaDashboardService.searchDefaultValues;
    }

    isAdvancedSearchMade() {
        return !!Object.values(this._opaDashboardService.opaRequestObject)
            .find(V => V && ((typeof (V) === 'string' && V) || (typeof (V) === 'object' && V.length)));
    }

    setDashboardTab() {
        this._opaDashboardService.opaRequestObject.tabType = sessionStorage.getItem('currentOPATab') ?
            sessionStorage.getItem('currentOPATab') : 'MY_DASHBOARD';
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
                    this.isLoading = false;
                }
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
        this.advanceSearchDates = { submissionDate: null };
        if (this._opaDashboardService.opaRequestObject.tabType !== 'ALL_DISCLOSURES') {
            this._opaDashboardService.isAdvanceSearch = false;
        }
        this.lookupValues = [];
        this.setSearchOptions();
    }

    performAdvanceSearch() {
        this.localOPARequestObject.currentPage = 1;
        this.setAdvanceSearchToServiceObject();
        this.isShowOPAList = true;
        this.opaList = [];
        this._opaDashboardService.isAdvanceSearch = true;
        this.$opaList.next();
    }

    setAdvanceSearchToServiceObject() {
        this._opaDashboardService.opaRequestObject.dispositionStatusCodes = this.localOPARequestObject.dispositionStatusCodes || [];
        this._opaDashboardService.opaRequestObject.opaDisclosureStatusCodes = this.localOPARequestObject.opaDisclosureStatusCodes || [];
        this._opaDashboardService.opaRequestObject.unitNumber = this.localOPARequestObject.unitNumber || null;
        this._opaDashboardService.opaRequestObject.submissionTimestamp = parseDateWithoutTimestamp(
            this.advanceSearchDates.submissionDate) || null;
        this._opaDashboardService.opaRequestObject.currentPage = this.localOPARequestObject.currentPage;
        this._opaDashboardService.searchDefaultValues.departmentName = this.localSearchDefaultValues.departmentName || null;
    }

    onLookupSelect(data: any, property: string) {
        this.lookupValues[property] = data;
        this.localOPARequestObject[property] = data.length ? data.map(d => d.code) : [];
    }

    redirectToDisclosure(opa) {
        this._router.navigate(['/coi/opa/form'],
            { queryParams: { disclosureId: opa.opaDisclosureId } });
    }

    getRequestObject() {
        this.localOPARequestObject.submissionTimestamp = parseDateWithoutTimestamp(this.advanceSearchDates.submissionDate);
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

}
