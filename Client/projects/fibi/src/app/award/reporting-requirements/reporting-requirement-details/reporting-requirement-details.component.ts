import { Component, EventEmitter, Input, OnChanges, Output, SimpleChanges } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { fileDownloader, openInNewTab, setFocusToElement } from '../../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { ReportingRequirementsService } from '../reporting-requirements.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { WafAttachmentService } from '../../../common/services/waf-attachment.service';
import { environment } from '../../../../environments/environment';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';
import { CommonDataService } from '../../services/common-data.service';

@Component({
    selector: 'app-reporting-requirement-details',
    templateUrl: './reporting-requirement-details.component.html',
    styleUrls: ['./reporting-requirement-details.component.css']
})
export class ReportingRequirementDetailsComponent implements OnChanges {

    @Input() reportStatusList = [];
    @Input() awardReport = {
        awardReportTracking: [],
        awardReportTermsId: null,
        frequencyCode: null,
        reportClassCode: null
    };
    @Input() awardData;
    @Input() isEditMode = false;
    @Input() rowData;
    @Input() isProgressReportEnabled;
    @Input() isReplaceAttachmentEnabled;
    @Input() isEditEnabledForSection;
    @Output() dataChange: EventEmitter<boolean> = new EventEmitter();
    @Output() refreshReport: EventEmitter<null> = new EventEmitter();
    @Output() cancelAddReminder: EventEmitter<boolean> = new EventEmitter();
    @Input() isAddNewReporting: boolean;

    datePlaceHolder = DEFAULT_DATE_FORMAT;
    setFocusToElement = setFocusToElement;
    deployMap = environment.deployUrl;
    mandatoryMsg = null;
    elasticSearchOptions: any = [];
    clearField: string;
    reportTrackingList: any = {};
    selectedIndex = null;
    isReplaceAttachment = false;
    isDeleteAttachment = false;
    attachmentVersions = [];
    fileName;
    editedRowCopy = null;
    editIndex = null;
    isRowEdit = [];
    uploadIndex = null;
    createIndex = null;
    isModifiable = false;
    isReportCreatable = false;
    isActiveReport = false;
    validateMap = new Map();
    isSaving = false;
    childReport: any = {awardReportTracking: [], awardReportTermsId: null, frequencyCode: null, reportClassCode: null};
    createReportDetails: any = {
        dueDate: null,
        awardNumber: null,
        reportClassCode: null,
        awardId: null,
        reportStartDate: null,
        reportEndDate: null
    };
    uniqueModalId: number;
    $subscriptions: Subscription[] = [];

    constructor(public _commonService: CommonService,
                private _commonData: CommonDataService,
                private _reportTermsService: ReportingRequirementsService,
                private _wafAttachmentService: WafAttachmentService,
                private _elasticConfig: ElasticConfigService) {
    }

    /**
     * Restricting to external data changes happening on childReport.(add/edit report changes)
     */
    ngOnChanges(changes: SimpleChanges): void {
        if (changes.awardReport && changes.awardReport.currentValue) {
            this.childReport = JSON.parse(JSON.stringify(this.awardReport));
            this.convertTimestampToDate();
            this.setElasticOptions(this.childReport.awardReportTracking);
            this.setUploadCreateIndex(this.childReport.awardReportTracking);
            this.cancelIfEditMode();
            this.setPermissions();
            this.checkIfReportIsActive(this.awardData);
        }
        if (this.isEditMode && this.isAddNewReporting) {
            this.childReport.awardReportTracking.unshift(this.addNewReportTracking());
            this.setElasticOptions(this.childReport.awardReportTracking);
            this.editRow(this.addNewReportTracking(), 0);
        }
    }
    /**
     * function creates a dummy object for new report tracking value
     */
    addNewReportTracking(): any {
        return {
            activityDate: null,
            awardId: this.awardData.awardId,
            awardNumber: this.awardData.awardNumber,
            awardProgressReport: null,
            awardReportTermsId: null,
            awardReportTrackingFile: null,
            awardReportTrackingId: null,
            comments: null,
            dueDate: null,
            preparerId: null,
            preparerName: null,
            progressReportId: null,
            sequenceNumber: this.awardData.sequenceNumber,
            statusCode: '1'
        };
    }

    /**
     * statusCode = 5 : Closed Award status.
     */
    checkIfReportIsActive({awardSequenceStatus = '', awardStatus = {statusCode: null}}) {
        if (awardSequenceStatus && awardStatus.statusCode) {
            const inactiveStatuses = ((awardSequenceStatus !== 'ACTIVE') ||
                (awardSequenceStatus === 'ACTIVE' && awardStatus.statusCode === '5'));
            this.isActiveReport = !inactiveStatuses;
        }
    }


    setPermissions() {
        this.isModifiable = (this.isEditMode &&  this.awardData.awardSequenceStatus === 'PENDING') ? true :
            this._commonData.checkDepartmentLevelRightsInArray('MAINTAIN_REPORTING_REQUIREMENTS');
        this.isReportCreatable = this._commonData.checkDepartmentLevelRightsInArray('CREATE_PROGRESS_REPORT');
    }

    convertTimestampToDate() {
        this.childReport.awardReportTracking.forEach(element => {
            element.dueDate = getDateObjectFromTimeStamp(element.dueDate);
            element.activityDate = getDateObjectFromTimeStamp(element.activityDate);
        });
    }

    /**
     * progressReportStatusCode : 4 => Approved.
     * @param awardReportTracking
     */
    setUploadCreateIndex(awardReportTracking: any) {
        this.uploadIndex = awardReportTracking.findIndex(item => !item.awardReportTrackingFile);
        if (this.isProgressReportEnabled && ['1', '2'].includes(this.childReport.reportClass.reportClassCode) && !this.isEditMode) {
            const isProgressReportPresent = awardReportTracking.findIndex(item => item.progressReportId) >= 0;
            if (isProgressReportPresent) {
                const lastIndexWithoutProgressReport = awardReportTracking.findIndex(item => !item.progressReportId);
                if (lastIndexWithoutProgressReport < 0) {
                    return this.createIndex = null;
                }
                const approvedReport = awardReportTracking[(lastIndexWithoutProgressReport - 1)]
                    .awardProgressReport.progressReportStatus.progressReportStatusCode === '4';
                this.createIndex = approvedReport ? lastIndexWithoutProgressReport : null;
            } else {
                this.createIndex = 0;
            }
        }
    }

    setElasticOptions(awardReportTrackList) {
        if (awardReportTrackList) {
            awardReportTrackList.forEach((element, index) => {
                this.elasticSearchOptions[index] = this._elasticConfig.getElasticForPerson();
                if (element.preparerName) {
                    this.elasticSearchOptions[index].defaultValue = element.preparerName;
                }
            });
        }
    }

    selectedPerson(event, index) {
        if (event !== null) {
            this.childReport.awardReportTracking[index].preparerId = event.prncpl_id;
            this.childReport.awardReportTracking[index].preparerName = event.full_name;
        } else {
            this.childReport.awardReportTracking[index].preparerId = null;
            this.childReport.awardReportTracking[index].preparerName = '';
        }
    }

    clearAttachmentDetails() {
        this.isReplaceAttachment = false;
    }

    toggleAttachmentModal() {
        document.getElementById('triggerAddAttachmentModal' + this.childReport.awardReportTermsId).click();
    }

    toggleVersionModal() {
        document.getElementById('triggerAttachmentVersionModal' + this.childReport.awardReportTermsId).click();
    }

    showUploadModal(index, reportTrackingList) {
        this.reportTrackingList = reportTrackingList;
        this.selectedIndex = index;
        this.toggleAttachmentModal();
    }

    addReportAttachment(files: any) {
        if (files.length === 1) {
            this.performAttachmentOperation(files[0]);
        }
    }

    private performAttachmentOperation(file: any): void {
        const requestObject = this.generateAttachmentRequestObject(file);
        this.generateRequestUsingStrategy(requestObject, file);
        if (this.isReplaceAttachment) { this.isReplaceAttachment = false; }
    }

    private generateRequestUsingStrategy(requestObject: any, file: any): void {
        if (!this._commonService.isWafEnabled) {
            this.uploadWithoutWaf(requestObject);
        } else {
            this.uploadWithWaf(requestObject, file);
        }
    }

    private generateAttachmentRequestObject(file: any): any {
        if (this.isDeleteAttachment) {
            return this.generateDeleteReqObj();
        }
        return this.generateAddOrReplaceReqObj(file);
    }

    private uploadWithoutWaf(requestObject: any): void {
        this.$subscriptions.push(this._reportTermsService.addAwardReportTrackingAttachment(requestObject).subscribe((data: any) => {
            if (this.isDeleteAttachment) {
                this.saveToAttachmentObject(null);
                return this.isDeleteAttachment = false;
            }
            this.saveToAttachmentObject(data.awardReportTrackingFiles[0]);
            this.setUploadCreateIndex(this.childReport.awardReportTracking);
            this.toggleAttachmentModal();
        }, _err => {
            if (this.isDeleteAttachment) { this.isDeleteAttachment = false; }
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to upload attachment, Please try again.');
        }));
    }

    private async uploadWithWaf(requestObject: any, file: any): Promise<void> {
        const wafRequestObject = {
            awardReportTrackingFile: requestObject,
            awardReportTrackingId: this.reportTrackingList.awardReportTrackingId,
            personId: this._commonService.getCurrentUserDetail('personID'),
            awardId: this.awardData.awardId,
            actionType: this.isDeleteAttachment ? 'D' : this.isReplaceAttachment ? 'R' : 'I'
        };
        const response = await this.performWafRequest(wafRequestObject, file);
        this.checkIfAttachmentSaved(response);
    }

    private async performWafRequest(wafRequestObject: any, file: any): Promise<any> {
        let response: any;
        if (this.isDeleteAttachment) {
            response = await this._wafAttachmentService.saveWafRequest(wafRequestObject, '/addAwardReportTrackingAttachmentForWaf');
        } else {
            response = await this._wafAttachmentService.saveAttachment(wafRequestObject, null, [file],
                '/addAwardReportTrackingAttachmentForWaf', null, null);
        }
        return response;
    }

    private checkIfAttachmentSaved(data: any): any {
        if (data && data.error) {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Waf blocked request for uploading the attachment');
            if (this.isDeleteAttachment) { return this.isDeleteAttachment = false; }
        }
        if (data && !data.error && !this.isDeleteAttachment) {
            this.saveToAttachmentObject(data.awardReportTrackingFile);
            this.setUploadCreateIndex(this.childReport.awardReportTracking);
        } else if (this.isDeleteAttachment) {
            this.saveToAttachmentObject(null);
            return this.isDeleteAttachment = false;
        }
        this.toggleAttachmentModal();
    }

    downloadReportAttachment(attachment: any) {
        this.$subscriptions.push(this._reportTermsService.downloadAwardReportTrackingAttachment(attachment.awardReportTrackingFileId)
            .subscribe(data => {
                fileDownloader(data, attachment.fileName);
            }, _err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Downloading reporting requirement attachment failed. Please try again.')));
    }

    deleteReportTracking(index: number) {
        this.$subscriptions.push(this._reportTermsService
            .deleteReportTracking(this.childReport.awardReportTracking[index].awardReportTrackingId).subscribe((res: any) => {
                this.rowData.awardReportTracking.splice(index, 1);
                this.childReport.awardReportTracking.splice(index, 1);
                this.setElasticOptions(this.childReport.awardReportTracking);
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entry deleted successfully.');
            }, err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to delete entry, Please try again.')));
    }

    saveReportTrackingDetails(reportTrackingList, index) {
        if (this.isEditMode && (!this.isFormValid(reportTrackingList) || this.isSaving)) {
            return;
        }
        this.isSaving = true;
        const requestObject = this.setDateFormat(reportTrackingList);
        requestObject.awardReportTermsId = this.childReport.awardReportTermsId;
        this.$subscriptions.push(this._reportTermsService.saveOrUpdateReportTracking({
            'awardReportTracking': requestObject
        }).subscribe((data: any) => {
            if (this.isAddNewReporting) {
                this.rowData.awardReportTracking.push(data.awardReportTracking);
                this.cancelAddReminder.emit(false);
            } else {
                this.rowData.awardReportTracking[index] = JSON.parse(JSON.stringify(requestObject));
            }
            this.childReport.awardReportTracking = this.sortAwardReportTracking(this.childReport.awardReportTracking);
            this.cancelEditRow(false);
            this.isSaving = false;
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entry saved successfully.');
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to save entry, Please try again.');
            this.isSaving = false;
        }));
    }

    sortAwardReportTracking(list: any[]): any[] {
        list.sort((a: any, b: any) => {
            let first: any, second: any;
            first = new Date(a.dueDate);
            second = new Date(b.dueDate);
            if (first < second) {
                return -1;
            }
            if (first > second) {
                return 1;
            }
            return 0;
        });
        return list;
    }

    isFormValid(reportTrackingList: any) {
        this.validateMap.clear();
        if (!reportTrackingList.dueDate) {
            this.validateMap.set('dueDate', 'Please enter a due date');
            return false;
        }
        return true;
    }

    setDateObject(awardReportTracking) {
        return {
            ...awardReportTracking,
            dueDate: getDateObjectFromTimeStamp(awardReportTracking.dueDate),
            activityDate: getDateObjectFromTimeStamp(awardReportTracking.activityDate)
        };
    }

    setDateFormat(reportTrackingList) {
        return {
            ...reportTrackingList,
            dueDate: parseDateWithoutTimestamp(reportTrackingList.dueDate),
            activityDate: parseDateWithoutTimestamp(reportTrackingList.activityDate)
        };
    }

    showAttachmentVersions(reportTrackingList: any) {
        this.attachmentVersions = [];
        this.fileName = reportTrackingList.awardReportTrackingFile.fileName;
        const awardReportTrackingId = reportTrackingList.awardReportTrackingFile.awardReportTrackingId;
        this.$subscriptions.push(this._reportTermsService
            .getReportTrackingAttachmentVersions(awardReportTrackingId).subscribe((res: any) => {
                this.attachmentVersions = res.attachmentVersions;
                this.toggleVersionModal();
            }));
    }

    navigateToReport(progressReportId: any) {
        openInNewTab('progress-report/overview?', ['progressReportId'], [progressReportId]);
    }

    editRow(reportTrackingList: any, index: number) {
        this.isRowEdit = [];
        this.isRowEdit[index] = true;
        if (typeof this.editIndex === 'number' && this.editedRowCopy) {
            this.restoreEditedRowData();
        }
        this.editedRowCopy = JSON.parse(JSON.stringify(reportTrackingList));
        this.editIndex = index;
    }

    cancelEditRow(restoreRowData = true) {
        this.isRowEdit = [];
        if (restoreRowData && typeof this.editIndex === 'number' && this.editedRowCopy) {
            this.restoreEditedRowData();
        }
        this.editIndex = null;
        this.editedRowCopy = {};
    }

    restoreEditedRowData() {
        this.childReport.awardReportTracking[this.editIndex] = this.editedRowCopy;
        this.setElasticOptions(this.childReport.awardReportTracking);
        this.validateMap.clear();
    }

    cancelIfEditMode() {
        if (typeof this.editIndex === 'number') {
            this.cancelEditRow();
        }
    }

    cancelAddNewReporting(): void {
        this.cancelEditRow();
        this.childReport.awardReportTracking.splice(0, 1);
        this.cancelAddReminder.emit(false);
    }

    createProgressReportRequestObject({awardId, awardNumber, sequenceNumber, dueDate, awardReportTrackingId},
                                      reportClassCode, frequencyCode) {
        this.createReportDetails = {
            title: this.awardData.title,
            dueDate, reportTrackingId: awardReportTrackingId, sequenceNumber,
            awardNumber, reportClassCode, awardId, reportStartDate: null, reportEndDate: null, frequencyCode
        };
    }

    runPostReportCreationProcesses(_progressId: number | null): void {
        this.uniqueModalId = null;
        this.setUploadCreateIndex(this.childReport.awardReportTracking);
        this.dataChange.emit(true);
        this.refreshReport.next();
    }

    public triggerConfirmDeleteAttachment(index: number, reportTrackingList: any): void {
        this.selectedIndex = index;
        this.reportTrackingList = reportTrackingList;
        this.cancelIfEditMode();
    }

    /**
     * Delete an attachment (if attachment id is present) or delete a single line item.
     */
    public performDelete(): void {
        if (this.isDeleteAttachment) {
            return this.performAttachmentOperation(null);
        }
        this.deleteReportTracking(this.selectedIndex);
    }

    private generateAddOrReplaceReqObj(file: any): FormData | any {
        const awardReportTrackingFiles: any = [];
        const awardReportTrackingFile: any = {};
        awardReportTrackingFile.awardReportTermsId = this.childReport.awardReportTermsId;
        awardReportTrackingFile.awardReportTrackingId = this.reportTrackingList.awardReportTrackingId;
        this.reportTrackingList.dueDate = parseDateWithoutTimestamp(this.reportTrackingList.dueDate);
        this.reportTrackingList.activityDate = parseDateWithoutTimestamp(this.reportTrackingList.activityDate);
        awardReportTrackingFile.awardId = this.awardData.awardId;
        awardReportTrackingFile.awardNumber = this.awardData.awardNumber;
        awardReportTrackingFile.sequenceNumber = this.awardData.sequenceNumber;
        awardReportTrackingFile.fileName = file ? file.name : null;
        awardReportTrackingFile.contentType = file ? file.type : null;
        awardReportTrackingFile.versionNumber = this.reportTrackingList.awardReportTrackingFile ?
            this.reportTrackingList.awardReportTrackingFile.versionNumber : null;
        awardReportTrackingFiles.push(awardReportTrackingFile);
        if (!this._commonService.isWafEnabled) {
            const formData = new FormData();
            formData.append('files', file, file.name);
            formData.append('formDataJson', JSON.stringify({
                'awardReportTrackingFiles': awardReportTrackingFiles,
                'awardReportTrackingId': this.reportTrackingList.awardReportTrackingId,
                'awardId': this.awardData.awardId,
                'actionType': this.isReplaceAttachment ? 'R' : 'I'
            }));
            return formData;
        } else {
            return awardReportTrackingFile;
        }
    }

    private generateDeleteReqObj(): FormData | null {
        if (this._commonService.isWafEnabled) {
            return null;
        }
        const formData = new FormData();
        formData.append('formDataJson', JSON.stringify({
            'awardReportTrackingId': this.reportTrackingList.awardReportTrackingId,
            'awardId': this.awardData.awardId,
            'actionType': 'D'
        }));
        return formData;
    }

    private saveToAttachmentObject(fileData: any): void {
        return this.childReport.awardReportTracking[this.selectedIndex].awardReportTrackingFile =
            this.rowData.awardReportTracking[this.selectedIndex].awardReportTrackingFile = fileData;
    }

}
