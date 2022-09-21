/**
 * Author: Ayush Mahadev R
 * Last updated by Ayush Mahadev R- 11-01-2021
 */
import {Component, OnDestroy, OnInit} from '@angular/core';
import {Subscription} from 'rxjs';
import {CommonDataService} from '../services/common-data.service';
import {subscriptionHandler} from '../../common/utilities/subscription-handler';
import {CommonService} from '../../common/services/common.service';
import {setFocusToElement} from '../../common/utilities/custom-utilities';
import {
    compareDates,
    compareDatesWithoutTimeZone,
    getDateObjectFromTimeStamp,
    parseDateWithoutTimestamp
} from '../../common/utilities/date-utilities';
import {ProgressReportMilestonesService} from './progress-report-milestones.service';
import {ActivatedRoute} from '@angular/router';
import {DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from '../../app-constants';

declare var $: any;

@Component({
    selector: 'app-progress-report-milestones',
    templateUrl: './progress-report-milestones.component.html',
    styleUrls: ['./progress-report-milestones.component.css']
})
export class ProgressReportMilestonesComponent implements OnInit, OnDestroy {

    progressReportId = null;
    datePlaceHolder = DEFAULT_DATE_FORMAT;
    setFocusToElement = setFocusToElement;
    actualStartMonth = null;
    actualEndMonth = null;
    isEditMode = false;
    isCommentView: boolean[] = [];
    isLineItemEdit: boolean[] = [];
    currentEditIndex: number = null;
    mandatoryList = new Map();
    tempMilestone: any = {};
    awardDates: any = {};
    milestoneObject: any = {
        awardProgressReportMilestones: [],
        progressReportMilestoneStatuses: []
    };
    milestoneStatusCode: any = null;
    column = '';
    direction: number = -1;
    isDesc: any;
    $subscriptions: Subscription[] = [];

    constructor(private _route: ActivatedRoute,
                public _commonData: CommonDataService,
                public _commonService: CommonService,
                private _milestoneService: ProgressReportMilestonesService) {
    }

    ngOnInit() {
        this.progressReportId = this._route.snapshot.queryParams['progressReportId'];
        this.getEditMode();
        this.getProgressReportData();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    getProgressReportData() {
        this.$subscriptions.push(this._commonData.getProgressReportData().subscribe((reportData: any) => {
            if (reportData && reportData.awardProgressReport && reportData.awardProgressReport.award) {
                this.awardDates.awardStartDate = reportData.awardProgressReport.award.beginDate;
                this.awardDates.awardEndDate = reportData.awardProgressReport.award.finalExpirationDate;
            }
        }));
    }

    getEditMode() {
        this.$subscriptions.push(this._commonData.getEditMode().subscribe((editMode: boolean) => {
            this.cancelEditIfAnyExists();
            this.isEditMode = editMode;
            this.loadMilestoneData(this.progressReportId);
        }));
    }

    loadMilestoneData(progressReportId) {
        this.$subscriptions.push(this._milestoneService.loadProgressReportMilestone(progressReportId).subscribe((res: any) => {
            this.milestoneObject = res;
        }, err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Loading milestone details failed. Please try again.')));
    }

    /**
     * Toggle Remarks/Comments sections to hide and show.Only one remark is allowed to open at a time.
     * @param editIndex - selected index number
     */
    commentAction(editIndex: number) {
        if (this._commonData.isDataChange) {
            $('#unSavedChangesModal').modal('show');
        } else {
            this.cancelEditIfAnyExists();
            if (this.isCommentView[editIndex]) {
                this.isCommentView[editIndex] = false;
            } else {
                this.showRemarksForIndex(editIndex);
            }
        }
    }

    showRemarksForIndex(index) {
        this.isCommentView = [];
        this.isCommentView[index] = true;
    }

    cancelEditIfAnyExists() {
        if (typeof(this.currentEditIndex) === 'number') {
            this.cancelEditMode(this.currentEditIndex);
        }
    }

    cancelEditMode(index, replaceOldData = true) {
        this.isLineItemEdit[index] = false;
        this._commonData.isDataChange = false;
        this.mandatoryList.clear();
        if (replaceOldData) {
            this.milestoneObject.awardProgressReportMilestones[index] = this.tempMilestone;
        }
        this.tempMilestone = {};
        this.currentEditIndex = null;
    }

    /**
     * Toggle edit mode for each line element.
     * Makes a temporary copy for cancel operation.
     * Clears all mandatory errors if any from previous edit.
     * populate editable fields with date and milestone status data.
     * @param milestone - select row data object
     * @param editIndex - selected index number
     */
    editMilestone(milestone: any, editIndex: number) {
        if (this._commonData.isDataChange) {
            $('#unSavedChangesModal').modal('show');
        } else {
            this.cancelEditIfAnyExists();
            this.currentEditIndex = editIndex;
            this.showRemarksForIndex(editIndex);
            this.isLineItemEdit = [];
            this.isLineItemEdit[editIndex] = true;
            this.tempMilestone = JSON.parse(JSON.stringify(milestone));
            this.mandatoryList.clear();
            this.setJavascriptDates(milestone.actualStartMonth, milestone.actualEndMonth);
            this.setMilestoneStatusCode(milestone.milestoneStatusCode);
        }
    }

    /**
     * Convert UNIX timestamps to javascript date objects to show in calendar picker
     * @param actualStartMonth
     * @param actualEndMonth
     */
    setJavascriptDates(actualStartMonth: any, actualEndMonth: any) {
        this.actualStartMonth = getDateObjectFromTimeStamp(actualStartMonth);
        this.actualEndMonth = getDateObjectFromTimeStamp(actualEndMonth);
    }

    /**
     * sets milestone status for edit mode.
     * @param milestoneStatusCode
     */
    setMilestoneStatusCode(milestoneStatusCode) {
        this.milestoneStatusCode = milestoneStatusCode;
    }

    checkMandatoryFields() {
        if (this.milestoneStatusCode === null || this.milestoneStatusCode === 'null') {
            this.mandatoryList.set('status', 'Please select a status');
        } else {
            this.mandatoryList.delete('status');
        }
    }

    updateMilestone(milestone: any) {
        this.checkMandatoryFields();
        if (this.mandatoryList.size === 0) {
            const requestObject = this.createRequestObject(milestone);
            this.$subscriptions.push(this._milestoneService.saveOrUpdateProgressReportMilestone(requestObject).subscribe((res: any) => {
                this.updateMilestoneEntry(res, milestone);
                this.cancelEditMode(this.currentEditIndex, false);
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Milestone Saved Successfully');
            }, err => this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to save milestone data! Please try again.')));
        }
    }

    createRequestObject({progressReportMilestoneId, awardId, awardNumber, sequenceNumber, milestoneNumber, remark}) {
        return {
            awardProgressReportMilestone: {
                progressReportMilestoneId, awardId, awardNumber, sequenceNumber, milestoneNumber, remark,
                awardProgressReport: {progressReportId: this.progressReportId},
                actualStartMonth: parseDateWithoutTimestamp(this.actualStartMonth),
                actualEndMonth: parseDateWithoutTimestamp(this.actualEndMonth),
                milestoneStatusCode: this.milestoneStatusCode,
                milestoneStatus: this.getMilestoneStatusByCode(this.milestoneStatusCode)
            }
        };
    }

    /**
     * Updates only specific return data from update api call.
     * @param awardProgressReportMilestone
     * @param milestone
     */
    updateMilestoneEntry({awardProgressReportMilestone}: any, milestone: any) {
        milestone.actualEndMonth = awardProgressReportMilestone.actualEndMonth;
        milestone.actualStartMonth = awardProgressReportMilestone.actualStartMonth;
        milestone.milestoneStatusCode = awardProgressReportMilestone.milestoneStatusCode;
        milestone.milestoneStatus = awardProgressReportMilestone.milestoneStatus;
    }

    /**
     * Returns entire milestone status object for a given milestoneStatusCode.
     * @param milestoneStatusCode
     */
    getMilestoneStatusByCode(milestoneStatusCode) {
        const milestoneStatus = this.milestoneObject.progressReportMilestoneStatuses
            .filter(status => status.milestoneStatusCode.toString() === milestoneStatusCode.toString());
        return milestoneStatus[0] ? milestoneStatus[0] : {};
    }

    /**
     * Returns an error if end date is before start date.
     */
    dateValidation() {
        this.mandatoryList.clear();
        if (this.actualStartMonth && this.actualEndMonth) {
            if (compareDates(this.actualStartMonth, this.actualEndMonth) === 1) {
                this.mandatoryList.set('actualEndMonth', 'Choose an actual end date after the actual start date');
            }
        }
        if (this.actualStartMonth &&
            compareDatesWithoutTimeZone(getDateObjectFromTimeStamp(this.awardDates.awardStartDate), this.actualStartMonth) === 1) {
            this.mandatoryList.set('actualStartMonth', 'Choose an actual start date on or after the award start date');
        }
        if (this.actualEndMonth &&
            compareDatesWithoutTimeZone(getDateObjectFromTimeStamp(this.awardDates.awardEndDate), this.actualEndMonth) === -1) {
            this.mandatoryList.set('actualEndMonth', 'Choose an actual end date on or before the award end date');
        }
    }

    saveModifiedChanges() {
        const milestone = this.milestoneObject.awardProgressReportMilestones[this.currentEditIndex];
        this.updateMilestone(milestone);
    }

    sortBy(property) {
        this.cancelEditIfAnyExists();
        this.column = property;
        this.direction = this.isDesc ? 1 : -1;
    }

}
