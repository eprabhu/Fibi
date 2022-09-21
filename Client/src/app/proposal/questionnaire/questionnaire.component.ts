import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';

import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { DataStoreService } from '../services/data-store.service';
import { CommonService } from '../../common/services/common.service';
import { ProposalService } from '../services/proposal.service';

@Component({
	selector: 'app-questionnaire-module',
	template: `<app-questionnaire [isViewMode]="isViewMode" [requestObject]="questionnaireObject"
	[dataVisibilityObj]="dataVisibilityObj"></app-questionnaire>`
})
export class QuestionnaireComponent implements OnInit, OnDestroy {

	$subscriptions: Subscription[] = [];
	dataDependencies = [ 'proposal', 'dataVisibilityObj', 'availableRights' ];
	dataVisibilityObj: any;
	proposal: any = {};
	isViewMode = false;
	questionnaireObject = {
		moduleSubitemCodes: [0],
		moduleItemKey: '',
		name: 'Questionnaire'
	};

	constructor(
		public _commonService: CommonService,
		public _proposalService: ProposalService,
		private _dataStore: DataStoreService
	) {}

	ngOnInit() {
		this.getDataFromStore();
		this.listenDataChangeFromStore();
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	private getDataFromStore() {
		const DATA = this._dataStore.getData(this.dataDependencies);
		this.proposal = DATA.proposal;
		this.dataVisibilityObj = DATA.dataVisibilityObj;
		this.isViewMode = this.checkQuestionnaireEditMode(DATA);
		this.questionnaireObject.moduleItemKey = this.proposal.proposalId;
	}

	private listenDataChangeFromStore() {
		this.$subscriptions.push(
			this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
				if (dependencies.some((dep) => this.dataDependencies.includes(dep))) {
					this.getDataFromStore();
				}
			})
		);
	}

	/**
	 * @param data
	 * @returns  questionnaire mode will be in edit even if the proposal is in viewmode. if the user has MODIFY_PROPOSAL_QUESTIONNAIRE right
	 * this right checking will be only available if the proposal is either 1- inprogress, 3- returned, 9-revison requested, 12 -withdrawn
	 * for simplying the logic i have checked isedimode and then the retrun is neagation
	 * since questionnaire engine requires isViewMode as input.
	 */
	private checkQuestionnaireEditMode(data) {
		const isEditMode =  ([3, 1, 9, 12].includes(data.proposal.statusCode) && data.availableRights.includes('MODIFY_PROPOSAL_QUESTIONNAIRE'));
		if (isEditMode) {
			return !isEditMode;
		}
		return this.dataVisibilityObj.mode === 'view';
	}

}
