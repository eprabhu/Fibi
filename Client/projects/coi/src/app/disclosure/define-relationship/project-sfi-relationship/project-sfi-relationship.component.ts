import { Component, Input, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { DefineRelationshipService } from '../services/define-relationship.service';
import { closeCommonModal } from '../../../common/utilities/custom-utilities';
import { ApplyToAllModal, COI, DefineRelationshipDataStore, ProjectSfiRelationConflictRO, ProjectSfiRelations } from '../../coi-interface';
import { ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { Subscription } from 'rxjs';
import { DefineRelationshipDataStoreService } from '../services/define-relationship-data-store.service';
import { DataStoreService } from '../../services/data-store.service';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-project-sfi-relationship',
    templateUrl: './project-sfi-relationship.component.html',
    styleUrls: ['./project-sfi-relationship.component.scss'],
})
export class ProjectSfiRelationshipComponent implements OnInit {

    projectSfiRelationsList: ProjectSfiRelations[] = [];
    @Input() isCardExpanded = true;
    isEditMode = true;
    @Input() i: number;
    coiData = new COI()

    isTableCollapse: boolean[] = [];
    visibleItems: any[] = []
    intersectionObserverOptions: IntersectionObserverInit;
    height: any;
    isStart = false;
    mandatoryList: Map<string, string> = new Map();
    $subscriptions: Subscription[] = [];

    constructor(private _dataStore: DataStoreService,
        private _commonService: CommonService,
        public defineRelationService: DefineRelationshipService,
        private _defineRelationsDataStore: DefineRelationshipDataStoreService) { }

    ngOnInit() {
        this.defineRelationService.isShowProjectSfiConflict[0] = true;
        this.listenDataChangeFromRelationStore();
        this.getDataFromStore();
        this.getDataFromRelationStore();
        this.listenDataChangeFromStore();
    }
    
    private setIntersectionObserver() {
        if (!this.isStart) {
            this.intersectionObserverOptions = {
                root: document.getElementById('SCROLL_SPY_LEFT_CONTAINER'),
                rootMargin: '200px 0px 200px 0px',
                threshold: 0.00
            };
            this.defineRelationService.updateObserverActivationStatus(this.projectSfiRelationsList.length, 0, true);
            setTimeout(() => {
                this.height = document.getElementById('COI_PROJECT_SFI_RELATION_0')?.getBoundingClientRect().height;
                this.isStart = true;
            }, 200);
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
            this._defineRelationsDataStore.relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId === 'All') {
                    this.getDataFromRelationStore();
                }
            })
        );
    }

    private getDataFromRelationStore(): void {
        this.projectSfiRelationsList = this._defineRelationsDataStore.getStoreData();
        this.setIntersectionObserver();

    }

    visibleInViewport(event: boolean, index: number): void {
        this.defineRelationService.isShowProjectSfiConflict[index] = event;
    }

    validateConflictStatus(): void {
        this.mandatoryList.delete('CONFLICT_STATUS');
        if (!this.defineRelationService.applyToAllModal.projectConflictStatusCode) {
            this.mandatoryList.set('CONFLICT_STATUS', 'Please select a conflict status.');
        }
    }

    validateDescription(): void {
        this.mandatoryList.delete('CONFLICT_COMMENT');
        if (!this.defineRelationService.applyToAllModal.comment) {
            this.mandatoryList.set('CONFLICT_COMMENT', 'Please enter the description.');
        }
    }

    private validateApplyToAll(): boolean {
        this.validateConflictStatus();
        this.validateDescription();
        return this.mandatoryList.size === 0;
    }

    applyToAllmodalAction(modalAction: ModalActionEvent) {
        switch (modalAction.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN':
                return this.clearApplyToAllModal();
            case 'PRIMARY_BTN':
                return this.saveProjectSfiConflict();
            default: break;
        }
    }

    clearApplyToAllModal(): void {
        closeCommonModal('coi-relation-modal');
        setTimeout(() => {
            this.mandatoryList.clear();
            this.defineRelationService.applyToAllModal = new ApplyToAllModal();
        }, 200);
    }

    private saveProjectSfiConflict(): void {
        if (this.validateApplyToAll()) {
            this.$subscriptions.push(
                this.defineRelationService.saveProjectSfiConflict(this.getProjectSfiRelationConflictRO())
                    .subscribe((res: ProjectSfiRelationConflictRO[]) => {
                        this.updateApplyToAllResponse(res);
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict status saved successfully.');
                    }, (_error: any) => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                    }));
        }
    }

    private getProjectSfiRelationConflictRO(): ProjectSfiRelationConflictRO {
        return new ProjectSfiRelationConflictRO({
            applyAll: true,
            relationshipSFIMode: false,
            personId: this.coiData.coiDisclosure.person.personId,
            disclosureId: this.coiData.coiDisclosure.disclosureId,
            comment: this.defineRelationService.applyToAllModal.comment,
            disclosureNumber: this.coiData.coiDisclosure.disclosureNumber,
            coiDisclProjectId: this.defineRelationService.applyToAllModal.coiDisclProjectId,
            projectConflictStatusCode: this.defineRelationService.applyToAllModal.projectConflictStatusCode
        });
    }


    private updateApplyToAllResponse(response: ProjectSfiRelationConflictRO[]): void {
        const PROJECT_SFI_RELATION = this.projectSfiRelationsList.find(type => type.coiDisclProjectId === this.defineRelationService.applyToAllModal.coiDisclProjectId);
        response.forEach((value: ProjectSfiRelationConflictRO, index: number) => {
            const COI_DISCL_ENT_PROJ_DETAILS = PROJECT_SFI_RELATION.coiDisclEntProjDetails[index];
            COI_DISCL_ENT_PROJ_DETAILS.disclComment.comment = value.comment;
            COI_DISCL_ENT_PROJ_DETAILS.disclComment.commentId = value.commentId;
            COI_DISCL_ENT_PROJ_DETAILS.projectConflictStatusCode = value.projectConflictStatusCode;
        });
        PROJECT_SFI_RELATION.conflictCount = this.defineRelationService.getFormattedConflictCount(PROJECT_SFI_RELATION.coiDisclEntProjDetails);
        this._defineRelationsDataStore.updateOrReplaceProject(PROJECT_SFI_RELATION, ['coiDisclEntProjDetails', 'conflictCount']);
        this.clearApplyToAllModal();
    }

}
