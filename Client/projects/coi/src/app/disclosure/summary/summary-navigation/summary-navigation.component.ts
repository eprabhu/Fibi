import { Component, OnInit } from '@angular/core';
import { DefineRelationshipService } from '../../define-relationship/services/define-relationship.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { DefineRelationshipDataStore, ProjectSfiRelations } from '../../coi-interface';
import { DefineRelationshipDataStoreService } from '../../define-relationship/services/define-relationship-data-store.service';
import { CoiService } from '../../services/coi.service';

@Component({
    selector: 'app-summary-navigation',
    templateUrl: './summary-navigation.component.html',
    styleUrls: ['./summary-navigation.component.scss']
})
export class SummaryNavigationComponent implements OnInit {

    $subscriptions: Subscription[] = [];
    projectSfiRelationsList: ProjectSfiRelations[] = [];
    filteredProjectSfiRelationsList: ProjectSfiRelations[] = [];

    constructor(public coiService: CoiService,
                public defineRelationshipService: DefineRelationshipService,
                private _defineRelationshipDataStore: DefineRelationshipDataStoreService) { }

    ngOnInit() {
        this.getDataFromRelationStore();
        this.listenDataChangeFromRelationStore();
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

    scrollIntoView(activeSectionId: 'COI801' | 'COI802' | 'COI803' | 'COI804'): void {
        if (!this.coiService.isExpandSummaryBySection[activeSectionId]) {
            setTimeout(() => {
                this.coiService.setActiveSection(activeSectionId);
                window.scroll(0, 0);
                const { leftOffsetTop } = this.defineRelationshipService.scrollSpyConfiguration;
                const SCROLL_SPY_LEFT_ITEM = document.getElementById(activeSectionId);
                const SCROLL_SPY_LEFT_CONTAINER = document.getElementById('SCROLL_SPY_LEFT_CONTAINER');
    
                if (SCROLL_SPY_LEFT_CONTAINER && SCROLL_SPY_LEFT_ITEM) {
                    SCROLL_SPY_LEFT_CONTAINER.scrollTo({
                        top: SCROLL_SPY_LEFT_ITEM.offsetTop - SCROLL_SPY_LEFT_CONTAINER.offsetTop - leftOffsetTop,
                        behavior: 'auto'
                    });
                }
            });
        }
    }

}
