/** Last updated by Ramlekshmy on 28-01-2020 */
// Last updated by Arun Raj(1.reduced lengthy function, 2.renamed some function names to meaningful name) on 07/04/2020
import { Component, OnDestroy, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { CommonDataService } from '../../services/common-data.service';
import { Subscription } from 'rxjs';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';
import { ReportingRequirementsService } from '../reporting-requirements.service';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { setFocusToElement, setHelpTextForSubItems } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { compareDatesWithoutTimeZone, getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { WafAttachmentService } from '../../../common/services/waf-attachment.service';


@Component({
    selector: 'app-reporting-requirements-edit',
    templateUrl: './reporting-requirements-edit.component.html',
    styleUrls: ['./reporting-requirements-edit.component.css'],
    providers: [WafAttachmentService]
})
export class ReportingRequirementsEditComponent implements OnInit, OnDestroy {

    awardApprovedEquipment: any = {};
    awardAprovedForeignTravel: any = {};
    awardReport: any = {
        awardReportTermRecipient: []
    };
    childReport: any = {
        awardReportTermRecipient: []
    };
    frequencyBaseTypeMsg: string = null;
    warningMsg: any = {};
    clearField: string;
    type: any;
    acType: any;
    index: any = {};
    specialType: string;
    awardData: any;
    reportsData: any = [];
    reportKeys: any[] = [];
    specialForeignTravel: any = [];
    specialEquipment: any = [];
    elasticSearchOptions: any = [];
    elasticPersonSearchOptions: any = {};
    reportTermsLookup: any = {};
    isEditReport = false;
    isInfoText = [true, true];
    isAddReport = true;
    isSpecialApproval = true;
    foreignTravelSum = 0;
    equipmentSum = 0;
    totalSum = 0;
    reportData;
    uploadedFile = [];
    currency;
    frequencyCode: any;
    frequencyBaseCode: any;
    frequenciesChanged = false;
    map = new Map();
    isShowFileDrop: any = [];
    reportTrackingList: any = {};
    clearRecipientField: any;
    isReports = false;
    isApproval = false;
    reportTypeName: string;
    datePlaceHolder = DEFAULT_DATE_FORMAT;
    setFocusToElement = setFocusToElement;
    mandatoryMsg = null;
    $subscriptions: Subscription[] = [];
    endingDate: Date;
    durInMonths: any;
    durInDays: any;
    durInYears: any;
    duedateError = false;
    isSaving = false;
    currentTab = '';
    isDetailsOpen = [];
    isProgressReportEnabled = false;
    isReplaceAttachmentEnabled = false;
    isEditEnabledForSection = false;
    selectedIndex;
    isDeletableIndex = [];
    isModifiable = false;
    helpText: any = { report: { reportClassCode: null, reportCode: null, parentHelpTexts: [] } };
    isAddNewReporting = [];

    constructor(private _reportTermsService: ReportingRequirementsService,
        public _commonService: CommonService,
        public _commonData: CommonDataService,
        private _elasticConfig: ElasticConfigService) {
    }

    ngOnInit() {
        this.currency = this._commonService.currencyFormat;
        this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
            if (data) {
                this.awardData = data.award;
                this.getReportTermsLookUp();
                this.getHelpText();
                this.setPermissions();
            }
        }));
    }

    getReportTermsLookUp() {
        if (this.awardData.awardId) {
            this.$subscriptions.push(this._reportTermsService.reportsTermsLookUpData(this.awardData.awardId)
                .subscribe((result: any) => {
                    this.reportTermsLookup = result;
                }));
            this.getReportsData();
        }
    }

    getHelpText() {
        this.$subscriptions.push(this._reportTermsService
            .fetchHelpText({ 'moduleCode': 1, 'sectionCodes': [109] }).subscribe((res: any) => {
                if (res) {
                    this.helpText = res;
                    this.setHelpTextForSubItems();
                }
            }));
    }

    setHelpTextForSubItems() {
        if (Object.keys(this.helpText).length && this.helpText.report && this.helpText.report.parentHelpTexts.length) {
            this.helpText = setHelpTextForSubItems(this.helpText, 'report');
            if (this.helpText.report['reportClassCode'] && this.helpText.report['reportClassCode'].parentHelpTexts.length > 0) {
                this.helpText.report = setHelpTextForSubItems(this.helpText.report, 'reportClassCode');
            }
            if (this.helpText.report['reportCode'] && this.helpText.report['reportCode'].parentHelpTexts.length > 0) {
                this.helpText.report = setHelpTextForSubItems(this.helpText.report, 'reportCode');
            }
        }
    }

    async setPermissions() {
        this.isModifiable = this.awardData.awardSequenceStatus === 'PENDING' ? true :
            await this._commonService.checkPermissionAllowed('MAINTAIN_REPORTING_REQUIREMENTS');
    }

    /**  Loads the report list. */
    getReportsData() {
        this.$subscriptions.push(this._reportTermsService.reportsData(this.awardData.awardId)
            .subscribe((data: any) => {
                this.reportsData = data.awardReportsList;
                this.isProgressReportEnabled = data.isProgressReportEnabled;
                this.isEditEnabledForSection = data.isEditEnabledForSection;
                this.isReplaceAttachmentEnabled = data.isReplaceAttachmentEnabled;
                if (this.reportsData) {
                    this.reportKeys = Object.keys(this.reportsData);
                    if (!this.currentTab) {
                        this.switchTabs(this.reportKeys[0]);
                    }
                    if (typeof this.selectedIndex === 'number') {
                        this.populateChildComponent(this.reportsData[this.currentTab][this.selectedIndex]);
                    }
                    this.setDeleteIndex(this.reportsData[this.currentTab]);
                } else {
                    this.reportKeys = [];
                }
            }));
    }


    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    setElasticForRecipients() {
        this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
    }

    /** restrict input fields to numbers, - and show validation.
     * @param event
     */
    inputRestriction(event: any) {
        const pattern = /[0-9\+\-\/\.\ ]/;
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    clearReports() {
        this.awardReport = {};
        this.awardReport.awardReportTermRecipient = [];
        this.clearRecipientField = new String('true');
    }

    /**
     * to set the details when the modal is opened
     */
    reportModalConfiguration() {
        this.isAddReport = true;
        this.isEditReport = false;
        this.awardReport.reportClassCode = null;
        this.awardReport.reportCode = '';
        this.awardReport.frequencyCode = '';
        this.awardReport.frequencyBaseCode = '';
        this.awardReport.ospDistributionCode = '';
        this.map.clear();
        this.awardReport.baseDate = null;
        this.warningMsg.recipientWarningText = null;
        this.duedateError = false;
    }

    /**
     * @param  {} event
     * To retrieve id and full name from elastic search result and assign to project team object
     */
    selectedRecipients(event) {
        if (event !== null) {
            this.checkForDuplicateRecipients(event);
            this.clearRecipientField = new String('true');
        }
    }

    checkForDuplicateRecipients(event) {
        const dupRecipientObject = this.findDuplicateRecipients(event);
        if (dupRecipientObject != null) {
            dupRecipientObject.acType === 'D' ? dupRecipientObject.acType = 'U' :
                this.warningMsg.recipientWarningText = '* Recipients already added';
        } else {
            this.setRecipientObjectToAddAwardDetails(event);
        }
    }

    findDuplicateRecipients(event: any) {
        return this.awardReport.awardReportTermRecipient.find(recipient => recipient.recipientId === event.prncpl_id);
    }

    setRecipientObjectToAddAwardDetails(event: any) {
        let recipientObject: any = {};
        recipientObject.recipientId = event.prncpl_id;
        recipientObject.acType = 'I';
        recipientObject.fullName = event.full_name;
        recipientObject = this.addAwardDetails(recipientObject);
        this.awardReport.awardReportTermRecipient.push(recipientObject);
        this.warningMsg.recipientWarningText = null;
    }

    /**
     * @param  {} reportsData
     * common function for setting award details.
     */
    addAwardDetails(reportsData) {
        reportsData.awardId = this.awardData.awardId;
        reportsData.awardNumber = this.awardData.awardNumber;
        reportsData.sequenceNumber = this.awardData.sequenceNumber;
        reportsData.updateUser = this._commonService.getCurrentUserDetail('userName');
        return reportsData;
    }

    deleteRecipient(recipient, index) {
        this.warningMsg.recipientWarningText = null;
        this.awardReport.awardReportTermRecipient.forEach(element => {
            if (element.recipientId === recipient.recipientId) {
                this.deleteRecipientBasedOnAcType(recipient, index);
            }
        });
    }

    deleteRecipientBasedOnAcType(recipient: any, index) {
        if (recipient.acType === 'I') {
            this.awardReport.awardReportTermRecipient.splice(index, 1);
        } else {
            this.awardReport.awardReportTermRecipient[index].acType = 'D';
        }
    }

    /**
     * @param  {} frequencyTypeCode
     *  get Frequency type Code and returns corresponding type description to the table list
     */
    getFrequencyType(frequencyTypeCode) {
        let frequencyType: any = {};
        if (this.reportTermsLookup.frequencyList && frequencyTypeCode) {
            frequencyType = this.reportTermsLookup.frequencyList.find(type => type.frequencyCode === frequencyTypeCode);
            return String(frequencyType.description);
        }
    }

    /**
     * @param  {} frequencyBasisCode
     * get Frequency basis Code and returns corresponding type description to the table list
     */
    getFrequencyBasis(frequencyBasisCode) {
        let frequencyBasis: any = {};
        if (this.reportTermsLookup.frequencyBaseList && frequencyBasisCode) {
            frequencyBasis = this.reportTermsLookup.frequencyBaseList.find(type => type.frequencyBaseCode === frequencyBasisCode);
            return String(frequencyBasis.description);
        }
    }

    saveReports() {
        this.reportsValidation();
        if (this.map.size < 1 && !this.isSaving) {
            this.setRequestReportDataObjectForSave();
        }
    }

    reportsValidation() {
        this.map.clear();
        const awardBeginDate = getDateObjectFromTimeStamp(this._commonData.beginDate);
        const awardFinalExpirationDate = getDateObjectFromTimeStamp(this._commonData.finalExpirationDate);
        if (!this.awardReport.reportClassCode || this.awardReport.reportClassCode === 'null') {
            this.map.set('class', 'Please select a report class');
        }
        if (!this.awardReport.frequencyCode || this.awardReport.frequencyCode === 'null') {
            this.map.set('frequencyCode', 'Please select a frequency code');
        }
        if (!this.awardReport.frequencyBaseCode || this.awardReport.frequencyBaseCode === 'null') {
            this.map.set('frequencyBaseCode', 'Please select a frequency base code');
        }
        if (!this.awardReport.baseDate || this.awardReport.baseDate === 'null') {
            this.map.set('isFrequencyAsRequired', this.awardReport.frequencyBaseCode === '6' ?
            'Please add a base date' : 'Base date cannot be empty. Please select a valid frequency base');
        }
        if (this.awardReport.baseDate && compareDatesWithoutTimeZone(this.awardReport.baseDate, awardBeginDate) === -1) {
            this.map.set('isFrequencyAsRequired', 'Please add a base date after award start date');
        }
        if (this.awardReport.baseDate && compareDatesWithoutTimeZone(this.awardReport.baseDate, awardFinalExpirationDate) === 1) {
            this.map.set('isFrequencyAsRequired', 'Please add a base date before award end date');
        }
    }

    setRequestReportDataObjectForSave() {
        const REQUESTREPORTDATA: any = {};
        REQUESTREPORTDATA.acType = this.getType(this.awardReport.awardReportTermsId);
        this.removeReportName(this.awardReport);
        this.awardReport.updateTimestamp = new Date().getTime();
        this.awardReport = this.addAwardDetails(this.awardReport);
        this.awardReport.dueDate = parseDateWithoutTimestamp(this.awardReport.dueDate);
        this.awardReport.baseDate = parseDateWithoutTimestamp(this.awardReport.baseDate);
        REQUESTREPORTDATA.mapOfDates = this.reportTermsLookup.mapOfDates;
        if (this.awardReport.frequencyBaseCode === '6') {
            this.reportTermsLookup.mapOfDates[6] = new Date(this.awardReport.baseDate).getTime();
        }
        REQUESTREPORTDATA.awardReport = this.awardReport;
        REQUESTREPORTDATA.frequenciesChanged = this.frequenciesChanged;
        this.saveCurrentReport(REQUESTREPORTDATA);
    }

    /** removes report name which is not needed to pass in Variation request headers
     * @param  {} reportData
     */
    removeReportName(reportData) {
        delete reportData.reportName;
    }

    /**
     * @param  {} typeId
     * get Id's and return ac type as SAVE if id is null, otherwise return type as UPDATE
     */
    getType(typeId) {
        this.acType = (typeId != null) ? 'U' : 'I';
        return this.acType;
    }

    saveCurrentReport(REQUESTREPORTDATA) {
        this.isSaving = true;
        this.$subscriptions.push(this._reportTermsService.maintainReports(REQUESTREPORTDATA)
            .subscribe((data: any) => {
                this.awardReport = data.awardReport;
                this.getReportsData();
                document.getElementById('close-report-modal').click();
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Report saved successfully.');
                this.isSaving = false;
            }, err => {
                this.isSaving = false;
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to save report, Please try again.');
            }));
    }

    editReport(reportList: any) {
        this.setElasticForRecipients();
        this.clearRecipientField = new String('true');
        this.awardReport = JSON.parse(JSON.stringify(reportList));
        this.frequencyCode = this.awardReport.frequencyCode;
        this.frequencyBaseCode = this.awardReport.frequencyBaseCode;
        this.setReportDueDateAndBaseDate();
        this.fetchReportType();
        this.awardReport.reportCode = reportList.reportCode != null ? reportList.reportCode : '';
        this.frequencyBaseChange();
        this.setElasticOptions(this.awardReport.awardReportTracking);
    }

    setReportDueDateAndBaseDate() {
        this.awardReport.dueDate = getDateObjectFromTimeStamp(this.awardReport.dueDate);
        this.awardReport.baseDate = getDateObjectFromTimeStamp(this.awardReport.baseDate);
    }

    /* fetches report type based on report class */
    fetchReportType() {
        this.awardReport.reportCode = '';
        if (this.awardReport.reportClassCode && this.awardReport.reportClassCode !== 'null') {
            this.$subscriptions.push(this._reportTermsService.fetchReportTypeByReportClass(this.awardReport.reportClassCode)
                .subscribe((data: any) => {
                    this.reportTermsLookup.reportList = data;
                }));
        }
    }

    /** When a frequency base is selected, frequency base code compares and set value to baseDate. */
    frequencyBaseChange() {
        this.map.delete('isFrequencyAsRequired');
        this.awardReport.baseDate = getDateObjectFromTimeStamp(this.reportTermsLookup.mapOfDates[this.awardReport.frequencyBaseCode]);
        if (this.awardReport.baseDate) {
            this.awardReport.baseDate = new Date(this.awardReport.baseDate);
        }
        if (this.isEditReport === true) {
            this.checkForFrequencyChange();
        }
    }

    checkForFrequencyChange() {
        if (this.awardReport.frequencyBaseCode !== this.frequencyBaseCode) {
            this.frequenciesChanged = true;
        } else {
            this.frequenciesChanged = false;
        }
    }

    deleteReport(deleteReportList: any) {
        this.$subscriptions.push(this._reportTermsService.maintainReports({
            'awardReport': deleteReportList,
            'acType': 'D'
        }).subscribe((data: any) => {
                this.selectedIndex = null;
                this.currentTab = '';
                this.getReportsData();
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Report deleted successfully.');
            }, err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to delete report, Please try again.')));
    }

    populateChildComponent(reportList: any) {
        this.childReport = JSON.parse(JSON.stringify(reportList));
        this.setElasticOptions(this.childReport.awardReportTracking);
        this.convertTimestampToDate(reportList.dueDate);
    }

    setElasticOptions(awardReportTrackList) {
        awardReportTrackList.forEach((element, index) => {
            this.elasticSearchOptions[index] = this._elasticConfig.getElasticForPerson();
            if (element.preparerName) {
                this.elasticSearchOptions[index].defaultValue = element.preparerName;
            }
        });
    }

    convertTimestampToDate(startingDueDate: any) {
        this.childReport.awardReportTracking.forEach(element => {
            element.dueDate = getDateObjectFromTimeStamp(element.dueDate);
            element.activityDate = getDateObjectFromTimeStamp(element.activityDate);
        });
    }

    selectedPerson(event, index) {
        if (event !== null) {
            this.awardReport.awardReportTracking[index].preparerId = event.prncpl_id;
            this.awardReport.awardReportTracking[index].preparerName = event.full_name;
        } else {
            this.awardReport.awardReportTracking[index].preparerId = null;
            this.awardReport.awardReportTracking[index].preparerName = '';
        }
    }

    setDateFormatWIthoutTimeStamp() {
        this.awardReport.awardReportTracking.forEach(element => {
            element.dueDate = parseDateWithoutTimestamp(element.dueDate);
            element.activityDate = parseDateWithoutTimestamp(element.activityDate);
        });
    }

    /** set frequenciesChanged flag true on frequency changes. */
    checkForFrequencyChanges() {
        if (this.isEditReport === true) {
            if (this.awardReport.frequencyCode !== this.frequencyCode) {
                this.frequenciesChanged = true;
            } else {
                this.frequenciesChanged = false;
            }
        }
    }

    switchTabs(tabName: string) {
        this.currentTab = tabName;
        this.isDetailsOpen = [];
        this.isAddNewReporting = [];
        this.selectedIndex = null;
        this.setDeleteIndex(this.reportsData[tabName]);
    }

    setDeleteIndex(tabData = []) {
        tabData.map((list, index) => {
            this.isDeletableIndex[index] =
                list.awardReportTracking.filter(item => (item.progressReportId)).length === 0;
        });
    }

    toggleReportDetails(reportCodeList: any, index: number) {
        this.isDetailsOpen[index] = !this.isDetailsOpen[index];
        if (this.isDetailsOpen[index]) {
            this.selectedIndex = index;
            this.populateChildComponent(reportCodeList);
            this.isAddNewReporting[index] = false;
        } else {
            this.selectedIndex = null;
        }
    }

    cancelAddReminder(event: boolean, index: number): void {
        this.isAddNewReporting[index] = event;
    }
}
