import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { InstituteProposal, InstProposal } from '../institute-proposal-interfaces';
import { DataStoreService } from '../services/data-store.service';

@Component({
	selector: 'app-other-information',
	template: `<div id ="ip-other-information-section">
                <app-custom-element *ngIf="instProposalId" [moduleItemKey]="instProposalId" [moduleCode]='2'
                [viewMode]="viewMode"></app-custom-element> </div>`,
})
export class OtherInformationComponent implements OnInit, OnDestroy {

	$subscriptions: Subscription[] = [];
	instProposalId: string = '';
	viewMode: string;
	generalDetails: InstProposal;

	constructor(private _dataStore: DataStoreService, private _route: ActivatedRoute) { }

	ngOnInit() {
		this.getAvailableRights();
		this.getDataStoreEvent();
	}

	getDataStoreEvent() {
		this.$subscriptions.push(this._dataStore.dataEvent
			.subscribe((data: string[]) => {
				if (data.includes('availableRights') || data.includes('instProposal')) {
					this.getAvailableRights();
				}
			}));
	}

	getAvailableRights() {
		const data: InstituteProposal = this._dataStore.getData(['availableRights', 'instProposal']);
		this.generalDetails = data.instProposal;
		this.instProposalId = this._route.snapshot.queryParamMap.get('instituteProposalId');
		this.viewMode = data.availableRights.includes('MODIFY_INST_PROPOSAL') && data.instProposal.proposalSequenceStatus === 'PENDING' ? 'edit' : 'view';
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

}
