import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { CommonService } from '../../common/services/common.service';
import { ProposalService } from '../services/proposal.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { DataStoreService } from '../services/data-store.service';

@Component({
    selector: 'app-milestone',
    templateUrl: './milestone.component.html',
    styleUrls: ['./milestone.component.css']
})
export class MilestoneComponent implements OnInit, OnDestroy {
    @Input() result: any = {};
    @Input() mode = null;
    proposalMileStone: any = {};
    mileStoneData: any = {};
    deleteMilestoneEntry: any = {};
    slicedMilestoneDescriptions;
    isDesc: any;
    column = 'startMonth';
    direction: number = -1;
    mandatoryList = new Map();
    isDataChangeFlag = false;
    isShowMilestone = true;
    isEditMilestone = false;
    $subscriptions: Subscription[] = [];
    editIndex: any;
    isSaving = false;
    hasUnsavedChanges = false;

    constructor(private _commonService: CommonService,
                private _router: Router,
                private _proposalService: ProposalService,
                private _autoSaveService: AutoSaveService,
                private _dataStore: DataStoreService
    ) {
    }

    ngOnInit() {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    /** function sets the object to save the milestone
     */
    setMilestoneObject() {
        const isValid = this.milestoneValidation();
        if (isValid && !this.isSaving) {
            this.isSaving = true;
            this.proposalMileStone.proposalId = this.result.proposal.proposalId;
            this.proposalMileStone.updateUser = this._commonService.getCurrentUserDetail('userName');
            this.proposalMileStone.updateTimeStamp = new Date().getTime();
            this.mileStoneData.proposalMileStone = this.proposalMileStone;
            this.mileStoneData.proposalId = this.result.proposal.proposalId;
            this.mileStoneData.updateUser = this._commonService.getCurrentUserDetail('userName');
            this.saveMilestone();
        }
    }

    saveMilestone() {
        this.$subscriptions.push(this._proposalService.saveUpdateMilestone(this.mileStoneData).subscribe((data: any) => {
                this.result.proposalMileStones = data;
                this._dataStore.updateStore(['proposalMileStones'], this.result);
                this.setUnsavedChanges(false);
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Adding Milestone failed. Please try again.');
                this.isSaving = false;
                this.isEditMilestone = false;
            },
            () => {
                if (this.isEditMilestone) {
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Milestone updated successfully.');
                } else {
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Milestone added successfully.');
                }
                this.proposalMileStone = {};
                this.isSaving = false;
                this.isEditMilestone = false;
            }));
    }

    /**
     * @param  {} proposalMileStone : instance from the array of milestones to be deleted
     */
    editMilestone(proposalMileStone, index) {
        this.isEditMilestone = true;
        this.editIndex = index;
        delete proposalMileStone.slicedMilestoneDescriptions;
        this.proposalMileStone = Object.assign({}, proposalMileStone);
        this.setUnsavedChanges(true);
    }

    /**
     * @param  {} proposalMileStoneId : milestone Id
     * @param  {} proposalId : proposal id
     * sets the object properties of the selected milestone for deletion.
     */
    setMilestoneDeletionObject(proposalMileStoneId, proposalId) {
        this.deleteMilestoneEntry.proposalMileStoneId = proposalMileStoneId;
        this.deleteMilestoneEntry.proposalId = proposalId;
        this.deleteMilestoneEntry.updateUser = this._commonService.getCurrentUserDetail('userName');

    }

    /**deleted the selected milestone */
    deleteMilestone() {
        this.$subscriptions.push(this._proposalService.deleteProposalMileStone(this.deleteMilestoneEntry).subscribe((data: any) => {
            this.result.proposalMileStones = data;
        },
        err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Deleting Milestone failed. Please try again.');
        },
        () => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Milestone deleted successfully.');
        }));
    }

    /** for validating the milestone
     */
    milestoneValidation() {
        this.mandatoryList.clear();
        if (!this.proposalMileStone.mileStone || (this.proposalMileStone.milestone && !this.proposalMileStone.milestone.trim())) {
            this.mandatoryList.set('mileStone', 'mileStone');
        }
        if (this.proposalMileStone.startMonth === null || this.proposalMileStone.startMonth === undefined) {
            this.mandatoryList.set('startMonth', 'startMonth');
        }
        if (!this.proposalMileStone.duration) {
            this.mandatoryList.set('duration', 'duration');
        }
        return this.mandatoryList.size === 0 ? true : false;
    }

    /**
     * @param  {any} event : restricts the values to numbers only
     */
    milestoneDurationInput(event: any) {
        const value = event.target.value;
        !parseInt(value, 10) ? this.proposalMileStone.duration = '' : this.proposalMileStone.duration = parseInt(value, 10);
    }

    /**
     * @param  {any} event
     * Restrict all special character only accept integers.
     */
    milestoneStartMonthInput(event: any) {
        const pattern = /[0-9]\d*/;
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    sortBy(property) {
        this.isDesc = !this.isDesc;
        this.column = property;
        this.direction = this.isDesc ? 1 : -1;
    }

    cancelMilestone() {
        this.setUnsavedChanges(false);
        this.proposalMileStone = {};
        this.isSaving = false;
        this.isEditMilestone = false;
    }

    setUnsavedChanges(flag: boolean) {
        if (this.hasUnsavedChanges !== flag) {
            this._autoSaveService.setUnsavedChanges('Milestone', 'milestoneProposal', flag);
        }
        this.hasUnsavedChanges = flag;
    }
}
