import { Component } from '@angular/core';
import { DefineRelationshipService } from '../../services/define-relationship.service';
import { ApplyToAllModal, CoiDisclEntProjDetail, ProjectSfiRelationConflictRO, ProjectSfiRelations, SaveProjectSfiConflict } from '../../../coi-interface';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../../../app-constants';
import { closeCommonModal } from '../../../../common/utilities/custom-utilities';
import { ModalActionEvent } from '../../../../shared-components/coi-modal/coi-modal.interface';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../../services/data-store.service';
import { CommonService } from '../../../../common/services/common.service';
import { DefineRelationshipDataStoreService } from '../../services/define-relationship-data-store.service';

@Component({
    selector: 'app-apply-to-all-modal',
    templateUrl: './apply-to-all-modal.component.html',
    styleUrls: ['./apply-to-all-modal.component.scss']
})
export class ApplyToAllModalComponent {

    $subscriptions: Subscription[] = [];
    mandatoryList: Map<string, string> = new Map();

    constructor(private _dataStore: DataStoreService,
                private _commonService: CommonService,
                public defineRelationshipService: DefineRelationshipService,
                private _defineRelationshipDataStore: DefineRelationshipDataStoreService) { }

    validateConflictStatus(): void {
        this.mandatoryList.delete('CONFLICT_STATUS');
        if (!this.defineRelationshipService.applyToAllModal.projectConflictStatusCode) {
            this.mandatoryList.set('CONFLICT_STATUS', 'Please select a conflict status.');
        }
    }

    validateDescription(): void {
        this.mandatoryList.delete('CONFLICT_COMMENT');
        if (!this.defineRelationshipService.applyToAllModal.comment) {
            this.mandatoryList.set('CONFLICT_COMMENT', 'Please enter the description.');
        }
    }

    private validateApplyToAll(): boolean {
        this.validateConflictStatus();
        this.validateDescription();
        return this.mandatoryList.size === 0;
    }

    applyToAllmodalAction(modalAction: ModalActionEvent): void {
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
            this.defineRelationshipService.applyToAllModal = new ApplyToAllModal();
        }, 200);
    }

    private saveProjectSfiConflict(): void {
        if (this.validateApplyToAll()) {
            this.$subscriptions.push(
                this.defineRelationshipService.saveProjectSfiConflict(this.getProjectSfiRelationConflictRO())
                    .subscribe((res: SaveProjectSfiConflict) => {
                        this.updateApplyToAllResponse(res.conflictDetails);
                        this.defineRelationshipService.updateDisclosureConflictStatus(res.disclConflictStatusType);
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict status saved successfully.');
                    }, (_error: any) => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                    }));
        }
    }

    private getProjectSfiRelationConflictRO(): ProjectSfiRelationConflictRO {
        const COI_DATA = this._dataStore.getData();
        return new ProjectSfiRelationConflictRO({
            applyAll: true,
            relationshipSFIMode: false,
            personId: COI_DATA.coiDisclosure.person.personId,
            disclosureId: COI_DATA.coiDisclosure.disclosureId,
            comment: this.defineRelationshipService.applyToAllModal.comment,
            disclosureNumber: COI_DATA.coiDisclosure.disclosureNumber,
            coiDisclProjectId: this.defineRelationshipService.applyToAllModal.coiDisclProjectId,
            projectConflictStatusCode: this.defineRelationshipService.applyToAllModal.projectConflictStatusCode
        });
    }

    private updateApplyToAllResponse(response: ProjectSfiRelationConflictRO[]): void {
        const PROJECT_SFI_RELATIONS_LIST: ProjectSfiRelations[]  = this._defineRelationshipDataStore.getFilteredStoreData();
        const PROJECT_SFI_RELATION = PROJECT_SFI_RELATIONS_LIST?.find(type => type.coiDisclProjectId === this.defineRelationshipService.applyToAllModal.coiDisclProjectId);
        PROJECT_SFI_RELATION?.coiDisclEntProjDetails?.forEach((coiDisclEntProjDetail: CoiDisclEntProjDetail) => {
            const UPDATE_DATA = response?.find((res: ProjectSfiRelationConflictRO) => res.coiDisclProjectEntityRelId === coiDisclEntProjDetail.coiDisclProjectEntityRelId);
            if (UPDATE_DATA) {
                coiDisclEntProjDetail.disclComment.comment = UPDATE_DATA.comment;
                coiDisclEntProjDetail.disclComment.commentId = UPDATE_DATA.commentId;
                coiDisclEntProjDetail.projectConflictStatusCode = UPDATE_DATA.projectConflictStatusCode;
            }
        });
        const { conflictCount, conflictCompleted } = this.defineRelationshipService.getFormattedConflictData(PROJECT_SFI_RELATION.coiDisclEntProjDetails)
        PROJECT_SFI_RELATION.conflictCount = conflictCount;
        PROJECT_SFI_RELATION.conflictCompleted = conflictCompleted;
        this._defineRelationshipDataStore.updateOrReplaceProject(PROJECT_SFI_RELATION, ['coiDisclEntProjDetails', 'conflictCount', 'conflictCompleted']);
        this.clearApplyToAllModal();
    }

}
