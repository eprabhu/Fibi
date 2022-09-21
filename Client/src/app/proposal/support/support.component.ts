import { Component, OnDestroy, OnInit } from '@angular/core';
import { DataStoreService } from '../services/data-store.service';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { ProposalService } from '../services/proposal.service';

@Component({
    selector: 'app-proposal-support',
    templateUrl: './support.component.html',
    styleUrls: ['./support.component.css']
})
export class SupportComponent implements OnInit, OnDestroy {

    $subscriptions = [];
    result: any = {};
    dataDependencies = ['proposal', 'reviewTypeCode', 'preReviewTypes', 'preReviewClarifications'];

    constructor(private _dataStore: DataStoreService,
                public proposalService: ProposalService) {
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getDataFromStore() {
        this.result = this._dataStore.getData(this.dataDependencies);
    }

    listenDataChangeFromStore() {
        this.$subscriptions.push(this._dataStore.dataEvent.subscribe(
            (dependencies: string[]) => {
                if (dependencies.some(dep => this.dataDependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            }));
    }

}
