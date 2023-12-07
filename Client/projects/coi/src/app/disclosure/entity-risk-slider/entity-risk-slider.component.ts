import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { EntityRiskSliderService } from './entity-risk-slider.service';
import { Subscription } from 'rxjs';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
import { environment } from '../../../environments/environment';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CoiSummaryEventsAndStoreService } from '../summary/coi-summary-events-and-store.service';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { closeSlider, openCommonModal, openSlider } from '../../common/utilities/custom-utilities';
import { CoiService } from '../services/coi.service';

@Component({
	selector: 'app-disclosure-risk-slider',
	templateUrl: './entity-risk-slider.component.html',
	styleUrls: ['./entity-risk-slider.component.scss']
})
export class EntityRiskSliderComponent implements OnInit {

	@Output() closePage: EventEmitter<any> = new EventEmitter<any>();
	@Output() riskChange: EventEmitter<any> = new EventEmitter<any>();
	@Input() disclosureDetails: any;
	@Input() projectDetails: any;
	isReadMore: boolean[] = [];
	$subscriptions: Subscription[] = [];
	riskLookup = [];
	riskCategoryCode = null;
	riskStatusType: any;
	riskComment: any;
	disclosureHistoryLogs: any = {};
	deployMap = environment.deployUrl;
	readMoreOrLess = false;
	riskValidationMap = new Map();
	helpText = [
		'Modify the risk level associated with the entity using the Risk level field.',
		'Provide an adequate reason for your decision in the description field provided.'
	]
	isStatusEdited = false;

	constructor(private _entityRiskSliderService: EntityRiskSliderService,
		public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
		public commonService: CommonService,
		public _dataFormatPipe: DateFormatPipeWithTimeZone,
        public coiService: CoiService) { }

	ngOnInit() {
		this.getRiskLookup();
		this.getDisclosureRiskHistory();
	}

	clearValidationOnValueChange(TYPE): void {
		TYPE === 'COMMENT' ? this.riskValidationMap.delete('comment') :  this.riskValidationMap.delete('riskLevelCode'), this.isStatusEdited = true;
	}

	private getRiskLookup(): void {
		this.$subscriptions.push(this._entityRiskSliderService.getRiskLookup().subscribe((data: any) => {
			this.riskLookup = data;
		}))
	}

	saveRisk(): void {
		if (this.checkForMandatory()) {
			this.$subscriptions.push(this._entityRiskSliderService.saveRisk(this.getRequestObject()).subscribe((data: any) => {
				this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk modified successfully');
				this.riskCategoryCode = null;
				this.riskComment = null;
				this.emitRiskChange(data);
				this.getDisclosureRiskHistory();
				this.isStatusEdited = false;
			}, err => {
				if (err.status === 405) {
					this.coiService.concurrentUpdateAction = 'Modify Risk';
				} else {
					this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in modifying risk');
				}
			}));
		}
	}

	private emitRiskChange(data): void {
		let riskCategory = this.riskLookup.find(ele => ele.riskCategoryCode == data.riskCategoryCode);
		this.riskChange.emit({ 'riskCategoryCode': data.riskCategoryCode, 'riskCategory': riskCategory })
	}

	private checkForMandatory(): boolean {
		this.riskValidationMap.clear();
		if (!this.riskCategoryCode || this.riskCategoryCode == 'null') {
			this.riskValidationMap.set('riskLevelCode', 'Please select a risk level');
		}
		if (!this.riskComment) {
			this.riskValidationMap.set('comment', 'Please add a reason');
		}
		if (this.riskCategoryCode == this.disclosureDetails.riskCategoryCode) {
			this.riskValidationMap.set('duplicateRisk', 'You are trying to update the risk with the current risk level of the disclosure.');
			this.riskValidationMap.delete('riskLevelCode');
		}
		return this.riskValidationMap.size === 0 ? true : false;
	}

	clearRiskChanges() {
		this.riskValidationMap.clear();
		this.riskCategoryCode = null;
		this.riskComment = null;
		this.isStatusEdited = false;
	}

	private getRequestObject(): any {
		return {
			'disclosureId': this.disclosureDetails.disclosureId,
			'disclosureNumber': this.disclosureDetails.disclosureNumber,
			'riskCategoryCode': this.riskCategoryCode,
			'revisionComment': this.riskComment
		}
	}

	private  getDisclosureRiskHistory(): void {
		this.$subscriptions.push(this._entityRiskSliderService.getDisclosureRiskHistory(
			{
				'disclosureId': this.disclosureDetails.disclosureId,
				'disclosureNumber': this.disclosureDetails.disclosureNumber,
				'actionTypeCode': 10
			}).subscribe((data: any) => {
				this.updateHistoryLogs(data);
				this.isReadMore = [];
				setTimeout(() => {
					openSlider('disclosure-entity-risk-slider');
				});
			}));
	}

	modalHeader(projectDetails): string {
		return `# ${projectDetails.moduleItemId} - ${projectDetails.title}`;
	}

	private updateHistoryLogs(data: any): void {
		if (data.length) {
			this.disclosureHistoryLogs = [];
			data.forEach((historyObj) => {
				const date = this._dataFormatPipe.transform(historyObj.updateTimestamp);
				this.disclosureHistoryLogs[date] = this.disclosureHistoryLogs[date] ? this.disclosureHistoryLogs[date] : [];
				this.disclosureHistoryLogs[date].push(historyObj);
			});
		}
	}

	getDisclosureTitleName(fcoiTypeCode: any): string {
		switch (fcoiTypeCode) {
			case '1':
				return 'FCOI';
			case '2':
				return 'Proposal';
			case '3':
				return 'Award';
			case '4':
				return 'FCOI';
		}
	}

	getColorBadges(disclosure): string {
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

	isEmptyHistory(): boolean {
		return isEmptyObject(this.disclosureHistoryLogs);
	}

	ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

	getWarningClass(typeCode): string {
        switch (typeCode) {
            case '3':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '1':
                return 'low-risk';
            default:
                return;
        }
    }

	sortNull() {return 0;}

	validateSliderClose() {
		(this.isStatusEdited || this.riskComment) ? openCommonModal('risk-conflict-confirmation-modal') : this.closeConflictSlider();
	}

	closeConflictSlider() {
		closeSlider('disclosure-entity-risk-slider');
		setTimeout(() => {
			this.closePage.emit();
		}, 500);
	}

	leavePageClicked() {
		setTimeout(() => {
			this.closeConflictSlider();
		}, 100);
	}
}
