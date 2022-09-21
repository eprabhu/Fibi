import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DATE_PICKER_FORMAT } from '../../../app-constants';
import { setFocusToElement } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { BudgetData, BudgetStatus, RateType } from '../ip-budget';
import { BudgetDataService } from '../services/budget-data.service';

declare var $: any;

@Component({
    selector: 'app-budget-overview',
    templateUrl: './budget-overview.component.html',
    styleUrls: ['./budget-overview.component.css']
})
export class BudgetOverviewComponent implements OnInit, OnDestroy {

    @Input() isViewMode = true;
    $subscriptions: Subscription[] = [];
    isBudgetOverviewWidgetOpen = true;
    datePlaceHolder = DATE_PICKER_FORMAT;
    setFocusToElement = setFocusToElement;
    budgetDatesValidation = new Map();
    budgetData: BudgetData;
    budgetStatus: Array<BudgetStatus>;
    rateTypes: Array<RateType> = [];
    helpText: any;
    isBudgetStatusComplete = false;
    tempBudgetTemplateId = null;
    campusFlagList: any = [
        { value: 'N', description: 'ON' },
        { value: 'F', description: 'OFF' },
        { value: 'D', description: 'BOTH' }
    ];
    isSaving = false;
    costShareStatus: any;
    dataDependencies = ['instituteProposalBudgetHeader', 'isBudgetMerge', 'isCampusFlagEnabled', 'isFundingSupportDeclarationRequired',
        'isKeyPersonMerge', 'isReplaceAttachmentEnabled', 'isShowCostShareAndUnderrecovery', 'isShowInKind', 'costSharingTypes',
        'isShowModifiedDirectCost', 'isSpecialReviewMerge',
        'isShowBudgetOHRatePercentage', 'enableCostShareStatus', 'costShareTypeCode', 'rateTypes'];

    constructor(private _budgetDataService: BudgetDataService) { }

    ngOnInit() {
        this.getBudgetFromStore();
        this.listenForDataUpdate();
        if (this.budgetData && this.budgetData.enableCostShareStatus && this.budgetData.costShareTypeCode) {
            this.getCostSharingTypeCode(this.budgetData.costShareTypeCode);
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private listenForDataUpdate(): void {
        this.$subscriptions.push(
            this._budgetDataService.ipBudgetData.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dataDependencies.includes(dep))) {
                    this.getBudgetFromStore();
                }
            })
        );
    }

    private getBudgetFromStore() {
        this.budgetData = this._budgetDataService.getBudgetData(this.dataDependencies);
    }

    getCostSharingTypeCode(code) {
        this.costShareStatus = this.budgetData.costSharingTypes.find(ele => ele.costSharingTypeCode == code).description;
    }
}
