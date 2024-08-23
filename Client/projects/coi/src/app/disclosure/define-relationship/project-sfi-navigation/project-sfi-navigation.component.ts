import { ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { DefineRelationshipService } from '../services/define-relationship.service';
import { DefineRelationshipDataStoreService } from '../services/define-relationship-data-store.service';
import { DefineRelationshipDataStore, ProjectSfiRelations } from '../../coi-interface';

@Component({
    selector: 'app-project-sfi-navigation',
    templateUrl: './project-sfi-navigation.component.html',
    styleUrls: ['./project-sfi-navigation.component.scss'],
})
export class ProjectSfiNavigationComponent implements OnInit, OnDestroy {

    projectSfiRelationsList: ProjectSfiRelations[] = [];
    IS_SFI_PROJECT_SWITCH = true;
    searchText = '';
    currentProject: any;
    currentTab: 'PROJECT' | 'SFI' = 'PROJECT';
    $subscriptions: Subscription[] = [];    

    constructor(public defineRelationService: DefineRelationshipService,
                private _defineRelationsDataStore: DefineRelationshipDataStoreService) { }
    
    ngOnInit(): void {
        this.getDataFromRelationStore();
        this.listenDataChangeFromRelationStore();
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationsDataStore.relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.updatedKeys.includes('conflictCount')) {
                    this.getDataFromRelationStore();
                }
            })
        );
    }

    private getDataFromRelationStore(): void {
        this.projectSfiRelationsList = this._defineRelationsDataStore.getStoreData();
    }


    getFilteredListForSearchWord(): void {

    }

    resetList(): void {
        this.searchText = '';
    }

    switchTab(tab: 'PROJECT' | 'SFI'): void {
        this.currentTab = tab;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
