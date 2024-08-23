import { Component, OnInit } from '@angular/core';
import { CommonService } from '../../../../common/services/common.service';
import { DefineRelationshipService } from '../../services/define-relationship.service';
import { AddConflictSlider, COI, DefineRelationshipDataStore, ProjectSfiRelations } from '../../../coi-interface';
import { Subscription } from 'rxjs';
import { DefineRelationshipDataStoreService } from '../../services/define-relationship-data-store.service';
import { DataStoreService } from '../../../services/data-store.service';
import { isEmptyObject } from '../../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CoiService } from '../../../services/coi.service';
import { HTTP_ERROR_STATUS } from '../../../../app-constants';

@Component({
    selector: 'app-project-sfi-relationship',
    templateUrl: './project-sfi-relationship.component.html',
    styleUrls: ['./project-sfi-relationship.component.scss'],
})
export class ProjectSfiRelationshipComponent implements OnInit {

    filteredProjectSfiRelationsList: ProjectSfiRelations[] = [];
    height: any;
    isStart = false;
    $subscriptions: Subscription[] = [];
    mandatoryList: Map<string, string> = new Map();
    loginPersonId = '';
    coiData = new COI()
    intersectionObserverOptions: IntersectionObserverInit;

    constructor(public coiService: CoiService,
                private _dataStore: DataStoreService,
                private _commonService: CommonService,
                public defineRelationshipService: DefineRelationshipService,
                private _defineRelationshipDataStore: DefineRelationshipDataStoreService) {
        // should be first always for height calulation
        this.defineRelationshipService.isShowProjectSfiConflict[0] = true;
    }

    ngOnInit() {
        this.getDataFromStore();
        this.getDataFromRelationStore();
        this.listenDataChangeFromStore();
        this.listenDataChangeFromRelationStore();
        this.loginPersonId = this._commonService.getCurrentUserDetail('personID');
        if (this.defineRelationshipService.isShowErrorToast) {
            this.defineRelationshipService.isShowErrorToast = false;
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
        }
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore(): void {
        const COI_DATA = this._dataStore.getData();
        if (isEmptyObject(COI_DATA)) { return; }
        this.coiData = COI_DATA;
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationshipDataStore.$relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId === 'All' || changes.searchChanged || changes.updatedKeys.includes('coiDisclEntProjDetails')) {
                    this.getDataFromRelationStore();
                }
            })
        );
    }

    private getDataFromRelationStore(): void {
        this.filteredProjectSfiRelationsList = this._defineRelationshipDataStore.getFilteredStoreData();
        this.defineRelationshipService.isLoading = false;
        this.setIntersectionObserver();
    }

    private setIntersectionObserver(): void {
        if (!this.isStart) {
            this.intersectionObserverOptions = {
                root: document.getElementById('SCROLL_SPY_LEFT_CONTAINER'),
                rootMargin: '200px 0px 200px 0px',
                threshold: Array.from({ length: 100 }, (_, i) => i / 100)
            };
            this.defineRelationshipService.updateObserverActivationStatus(this.filteredProjectSfiRelationsList.length, 0, true);
            setTimeout(() => {
                this.height = document.getElementById('COI_PROJECT_SFI_RELATION_0')?.getBoundingClientRect().height;
                this.isStart = true;
            }, 200);
        }
    }

    visibleInViewport(event: { isVisible: boolean; observerEntry: IntersectionObserverEntry }, projectIndex: number): void {
        const SCROLL_SPY_INDEX = this.defineRelationshipService.isEditMode ? projectIndex : (projectIndex + 2);
        this.defineRelationshipService.isShowProjectSfiConflict[projectIndex] = event.isVisible;
        this.defineRelationshipService.updateScrollSpyConfig(event, SCROLL_SPY_INDEX);
    }

    closeAddConflictSlider(event: string): void {
        this.defineRelationshipService.addConflictSlider = new AddConflictSlider();
    }

}
