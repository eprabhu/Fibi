import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Subscription } from 'rxjs';
import { EntityDetailsService } from '../../disclosure/entity-details/entity-details.service';
import { CoiEntity, EntityDetails, RiskHistoryRO } from '../../entity-management/entity-details-interface';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
@Component({
    selector: 'app-entity-risk-slider',
    templateUrl: './entity-risk-slider.component.html',
    styleUrls: ['./entity-risk-slider.component.scss'],
})
export class EntityRiskSliderComponent implements OnInit {
    @Input() isVisible = false;
    @Input() isOpenSlider = true;
    @Input() entityDetails: CoiEntity = new CoiEntity();
    @Input() Risk: any;
    @Output() closePage: EventEmitter<any> = new EventEmitter;
    riskLevelLookup = [];
    $subscriptions: Subscription[] = [];
    riskLevelChanges = [];
    coiConflictStatusType = [];
    isReadMore = false;
    riskValidationMap = new Map();
    entityRiskRO: RiskHistoryRO = new RiskHistoryRO;
    isStatusEdited = false;
    riskHistoryLogs: any = {};
    currentRiskCategorycode: any;
    revisionComment: any;
    helpText = [
        'Modify the Risk of this Entity from the Risk field.',
        'Provide an adequate reason for your decision in the description field provided.'
    ];
    riskCategoryCode: string;

    constructor(public entityDetailsService: EntityDetailsService,
        private _commonService: CommonService,
        public dataFormatPipe: DateFormatPipeWithTimeZone) { }


    ngOnInit() {
        this.getSFILookup();
        this.riskHistory();
        setTimeout(() => {
            openSlider('risk-conflict-slider');
        });
    }

    openConformationModal() {
        document.getElementById('risk-conflict-confirmation-modal-trigger-btn').click();
    }

    validateSliderClose() {
        (this.isStatusEdited || this.revisionComment) ? this.openConformationModal() : this.closeConflictSlider();
    }

    closeConflictSlider() {
        closeSlider('risk-conflict-slider');
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    leavePageClicked(event: boolean) {
        if (event) {
            setTimeout(() => {
                this.closeConflictSlider();
            }, 100);
        }
    }

    sortNull() { return 0; }


    private getSFILookup(): void {
        this.$subscriptions.push(this.entityDetailsService.loadSFILookups().subscribe((res: any) => {
            this.riskLevelLookup = res.entityRiskCategories;
            this.currentRiskCategorycode = this.entityDetails?.riskCategoryCode;
        }));
    }


    clearConflictModal() {
        this.riskValidationMap.clear();
        this.entityRiskRO = new RiskHistoryRO;
        this.revisionComment = '';
        this.isStatusEdited = false;
        this.currentRiskCategorycode  = this.entityDetails.riskCategoryCode;
    }

    private getEntityRiskRO(): RiskHistoryRO {
        this.entityRiskRO.entityId = this.entityDetails?.entityId;
        this.entityRiskRO.entityNumber = this.entityDetails?.entityNumber;
        this.entityRiskRO.riskCategoryCode = this.currentRiskCategorycode;
        this.entityRiskRO.revisionReason = this.revisionComment;
        return this.entityRiskRO;
    }

    updateProjectRelationship() {
        if (this.checkForMandatory()) {
            this.$subscriptions.push(
                this.entityDetailsService.entityRisk(this.getEntityRiskRO())
                    .subscribe((data: any) => {
                        this.coiConflictStatusType = data;
                        this.entityDetails.revisionReason = this.revisionComment;
                        this.entityDetails.riskCategoryCode = this.currentRiskCategorycode;
                        this.clearConflictModal();
                        this.riskHistory();
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict updated successfully.');
                    }, _err => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating conflict status. Please try again.');
                    }));
        }
        this.checkForMandatory();
    }

    riskHistory() {
        this.$subscriptions.push(this.entityDetailsService.riskHistory(this.entityDetails.entityId).subscribe((data: any) => {
            this.updateHistoryLogs(data);
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
        }));

    }
    updateHistoryLogs(data: any) {
        if (data.length) {
            this.riskHistoryLogs = [];
            data.forEach((historyObj) => {
                const date = this.dataFormatPipe.transform(historyObj.updateTimestamp);
                this.riskHistoryLogs[date] = this.riskHistoryLogs[date] ? this.riskHistoryLogs[date] : [];
                this.riskHistoryLogs[date].push(historyObj);
            });
        }
    }
    closeHistoryInfo() {
        this.entityDetailsService.isShowHistoryInfo = false;
    }

    setCoiProjConflictStatusType(TYPE): void {
        TYPE === 'COMMENT' ? this.riskValidationMap.delete('comment') :  this.riskValidationMap.delete('riskLevelCode');
        this.isStatusEdited = true;
    }

    public checkForMandatory(): boolean {
        this.riskValidationMap.clear();
        if (!this.currentRiskCategorycode || this.currentRiskCategorycode === 'null') {
            this.riskValidationMap.set('riskLevelCode', 'Please select a risk level');
        }
        if (!this.revisionComment) {
            this.riskValidationMap.set('comment', 'Please add a reason.');
        }
        if (this.currentRiskCategorycode  === this.entityDetails.riskCategoryCode) {
            this.riskValidationMap.set('duplicateRisk', 'You are trying to update the risk with the current risk level of the disclosure.');
            this.riskValidationMap.delete('riskLevelCode');
        }
        return this.riskValidationMap.size === 0 ? true : false;
    }
}

