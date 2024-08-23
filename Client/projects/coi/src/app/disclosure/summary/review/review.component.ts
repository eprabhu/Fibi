import { Component, OnDestroy, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../services/coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { CoiService } from '../../services/coi.service';
import { CoiSummaryService } from '../services/coi-summary.service';
import { DefineRelationshipDataStore, ProjectSfiRelations } from '../../coi-interface';
import { DefineRelationshipService } from '../../define-relationship/services/define-relationship.service';
import { heightAnimation } from '../../../common/utilities/animations';
import { DefineRelationshipDataStoreService } from '../../define-relationship/services/define-relationship-data-store.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.scss'],
    animations: [heightAnimation('0', '*', 400, 'heightAnimation')]
})
export class ReviewComponent implements OnInit, OnDestroy {

    isRelationCollapsed = true;
    $subscriptions: Subscription[] = [];
    intersectionObserverOptions: IntersectionObserverInit;
    isActivateObserverOption = false;
    projectSfiRelationsList: ProjectSfiRelations[] = [];
    filteredProjectSfiRelationsList: ProjectSfiRelations[] = [];

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService, public commonService: CommonService, public coiService: CoiService,
        public coiSummaryService: CoiSummaryService, public defineRelationshipService: DefineRelationshipService,
        private _defineRelationshipDataStore: DefineRelationshipDataStoreService
    ) { }

    ngOnInit() {
        this.getDataFromRelationStore();
        this.listenDataChangeFromRelationStore();
        this.intersectionObserverOptions = {
            root: document.getElementById('SCROLL_SPY_LEFT_CONTAINER'),
            rootMargin: '0px 0px 0px 0px',
            threshold: Array.from({ length: 100 }, (_, i) => i / 100)
        };
        this.isActivateObserverOption = true;

    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationshipDataStore.$relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.updatedKeys.includes('conflictCount') || changes.searchChanged) {
                    this.getDataFromRelationStore();
                }
            })
        );
    }

    private getDataFromRelationStore(): void {
        this.filteredProjectSfiRelationsList = this._defineRelationshipDataStore.getFilteredStoreData();
        this.projectSfiRelationsList = this._defineRelationshipDataStore.getActualStoreData();
    }

}
