import { ChangeDetectionStrategy, ChangeDetectorRef, Component, Input } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { coiReviewComment } from '../../../shared-components/shared-interface';
import { CoiService } from '../../services/coi.service';
import { DataStoreService } from '../../services/data-store.service';
import { DefineRelationshipDataStore, ProjectSfiRelations } from '../../coi-interface';
import { CONFLICT_STATUS_BADGE } from '../../../app-constants';
import { Subscription } from 'rxjs';
import { DefineRelationshipDataStoreService } from '../services/define-relationship-data-store.service';
import { DefineRelationshipService } from '../services/define-relationship.service';

@Component({
    selector: 'app-project-details-card',
    templateUrl: './project-details-card.component.html',
    styleUrls: ['./project-details-card.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class ProjectDetailsCardComponent {

    @Input() projectSfiRelation = new ProjectSfiRelations();

    $subscriptions: Subscription[] = [];
    CONFLICT_STATUS_BADGE = CONFLICT_STATUS_BADGE;

    constructor(public commonService: CommonService,
        private _cdr: ChangeDetectorRef,
        private _defineRelationService: DefineRelationshipService,
        private _defineRelationsDataStore: DefineRelationshipDataStoreService,
    ) { }

    ngOnInit() {
        this.listenDataChangeFromRelationStore();
    }

    private listenDataChangeFromRelationStore(): void {
        this.$subscriptions.push(
            this._defineRelationsDataStore.relationsChanged.subscribe((changes: DefineRelationshipDataStore) => {
                if (changes.projectId == 'All' || changes.projectId == this.projectSfiRelation.projectId) {
                    this._cdr.markForCheck();
                }
            })
        );
    }

    openReviewerComment(): void {
        this._defineRelationService.openReviewerComment(this.projectSfiRelation, 'RELATIONSHIP');
    }

    collapse(): void {

    }

}
