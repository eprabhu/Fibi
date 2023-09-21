import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { closeSlider, openCommonModal, openSlider } from '../../common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { Subscription } from 'rxjs';
import { TravelRiskSliderService } from './travel-risk-slider.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { TravelDataStoreService } from '../services/travel-data-store.service';

@Component({
  selector: 'app-travel-risk-slider',
  templateUrl: './travel-risk-slider.component.html',
  styleUrls: ['./travel-risk-slider.component.scss']
})
export class TravelRiskSliderComponent implements OnInit {

  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
  @Output() riskChange: EventEmitter<any> = new EventEmitter<any>();
  @Input() travelDisclosure: any;
  @Input() isEditMode = false;
	helpTexts = [
		'Modify the risk level associated with the travel using the Risk level field.',
		'Provide an adequate reason for your decision in the description field provided.'
	]
  riskCategoryCode = null;
  riskValidationMap = new Map();
  riskComment: any;
  riskLookup = [];
  $subscriptions: Subscription[] = [];
  disclosureHistoryLogs: any = {};
  isReadMore: boolean[] = [];
  isStatusEdited = false;
  
  constructor( public commonService: CommonService, 
			   public travelRiskSliderService: TravelRiskSliderService,
			   public dataFormatPipe: DateFormatPipeWithTimeZone,
			   private _dataStore: TravelDataStoreService ) { }

  ngOnInit() {
    setTimeout(() => {
      openSlider('travel-risk-slider');
    });
	this.getRiskLookup();
	this.getTravelDisclosureHistory();
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

	private checkForMandatory(): boolean {
		this.riskValidationMap.clear();
		if (!this.riskCategoryCode || this.riskCategoryCode == 'null') {
			this.riskValidationMap.set('riskLevelCode', 'Please select a risk level');
		}
		if (!this.riskComment) {
			this.riskValidationMap.set('comment', 'Please add a reason');
		}
		if (this.riskCategoryCode == this.travelDisclosure.riskCategoryCode) {
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

	clearValidationOnValueChange(TYPE): void {
		TYPE === 'COMMENT' ? this.riskValidationMap.delete('comment') :  this.riskValidationMap.delete('riskLevelCode'), this.isStatusEdited = true;
	}
  
	private getRiskLookup(): void {
		this.$subscriptions.push(this.travelRiskSliderService.getTravelRiskLookup().subscribe((data: any) => {
			this.riskLookup = data;
		}))
	}

	private getRequestObject(): any {
		return {
			'travelDisclosureId': this.travelDisclosure.travelDisclosureId,
			'travelNumber': this.travelDisclosure.travelNumber,
			'riskCategoryCode': this.riskCategoryCode,
			'comment': this.riskComment
		}
	}

	private emitRiskChange(data): void {
		this.riskChange.emit({ 'riskCategoryCode': data.riskCategoryCode, 'riskLevel': data.riskLevel })
	}

	saveRisk(): void {
		if (this.checkForMandatory()) {
			this.$subscriptions.push(this.travelRiskSliderService.saveRisk(this.getRequestObject()).subscribe((data: any) => {
				this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk modified successfully');
				this.travelDisclosure.riskCategoryCode = data.riskCategoryCode;
                this.travelDisclosure.riskLevel = data.riskLevel;
                this._dataStore.manualDataUpdate(this.travelDisclosure);
				this.riskCategoryCode = null;
				this.riskComment = null;
				this.emitRiskChange(data);
				this.getTravelDisclosureHistory();
				this.isStatusEdited = false;
			}, err => {
				this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in modifying risk');
			}));
		}
	}

	private  getTravelDisclosureHistory(): void {
		this.$subscriptions.push(this.travelRiskSliderService.getTravelRiskHistory(
			{
				travelDisclosureId: this.travelDisclosure.travelDisclosureId,
				actionTypeCode: 10
			}).subscribe((data: any) => {
				this.updateHistoryLogs(data);
				this.isReadMore = [];
				setTimeout(() => {
					openSlider('travel-risk-slider');
				});			
			}));
	}

	private updateHistoryLogs(data: any): void {
		if (data.length) {
			this.disclosureHistoryLogs = [];
			data.forEach((historyObj) => {
				const date = this.dataFormatPipe.transform(historyObj.updateTimestamp);
				this.disclosureHistoryLogs[date] = this.disclosureHistoryLogs[date] ? this.disclosureHistoryLogs[date] : [];
				this.disclosureHistoryLogs[date].push(historyObj);
			});
		}
	}

	isEmptyHistory(): boolean {
		return isEmptyObject(this.disclosureHistoryLogs);
	}

	
	getWarningClass(typeCode): string {
        switch (typeCode) {
            case '1':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '3':
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
        closeSlider('travel-risk-slider');
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