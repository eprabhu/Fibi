import {Component, Input, Output, OnChanges, OnDestroy, SimpleChanges, EventEmitter} from '@angular/core';
import {RelationshipService} from '../relationship.service';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {DisclosureSFIs, PersonEntity} from './sfi-project-interface';
import {CommonService} from '../../../common/services/common.service';
import {listAnimation} from '../../../common/utilities/animations';
import {slideHorizontal} from '../../../../../../fibi/src/app/common/utilities/animations';
import {environment} from '../../../../environments/environment';
import {CoiService} from '../../services/coi.service';

@Component({
    selector: 'app-define-sfi-project',
    templateUrl: './define-sfi-project.component.html',
    styleUrls: ['./define-sfi-project.component.scss'],
    animations: [slideHorizontal, listAnimation],
})
export class DefineSfiProjectComponent implements OnChanges, OnDestroy {

    @Input() disclosureId = null;
    @Input() disclosureStatusCode = null;
    @Input() coiStatusList = [];
    @Input() coiData: any;
    @Input() isEditMode = false;
    @Output() conflictStatusChanged = new EventEmitter();

    deployMap = environment.deployUrl;
    disclosureSfi: DisclosureSFIs = new DisclosureSFIs();
    readMore = [];
    isShowCollapsedConflictRelationship = false;
    isShowSlider = false;
    relationshipTypeCache = {};
    projectIdTitleMap = {};
    coiStatusCode = null;
    selectedSfi = {};
    selectedSfiIndex = -1;
    $subscriptions: Subscription[] = [];

    constructor(public relationshipService: RelationshipService, private _commonService: CommonService, public coiService: CoiService) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.disclosureId.previousValue !== changes.disclosureId.currentValue) {
            this.getSFIDetails();
        }
    }

    getSFIDetails() {
        if (this.disclosureId) {
            this.$subscriptions.push(this.relationshipService.getSFIOfDisclosure(this.disclosureId)
                .subscribe((res: DisclosureSFIs) => {
                    this.disclosureSfi = res;
                    this.isShowCollapsedConflictRelationship = this.disclosureSfi.personEntities.length === 1;
                    this.getProjectRelations();
                    this.getProjectsForEntity();
                }));
        }
    }

    getProjectRelations() {
        this.$subscriptions.push(this.relationshipService
            .getProjectRelations(this.disclosureId, this.disclosureStatusCode).subscribe((res: any) => {
                res.awards.forEach(award => this.projectIdTitleMap[award.moduleItemId] = `#${award.moduleItemKey} - ${award.title}`);
                res.proposals.forEach(proposal => this.projectIdTitleMap[proposal.moduleItemId] = `#${proposal.moduleItemId} - ${proposal.title}`);
            }));
    }

    getProjectsForEntity() {
        this.disclosureSfi.personEntities.forEach((sfi: PersonEntity) => {
            this.$subscriptions.push(this.relationshipService
                .getProjectsForEntity(this.disclosureId, sfi.personEntityId).subscribe((res: any) => {
                    this.relationshipService.projectSFIDetails[sfi.personEntityId] = res;
                }));
        });
    }

    readMoreOption(id: number, flag: boolean): void {
        this.readMore[id] = !flag;
    }

    getEntityRelationTypePills(validPersonEntityRelType: string) {
        if (this.relationshipTypeCache[validPersonEntityRelType]) {
            return this.relationshipTypeCache[validPersonEntityRelType];
        }
        const entityRelTypes = validPersonEntityRelType.split(':;:');
        this.relationshipTypeCache[validPersonEntityRelType] = entityRelTypes.map(entity => {
            const relationshipType = entity.split(':');
            return {relationshipType: relationshipType[0] || '', description: relationshipType[1] || ''};
        });
        return this.relationshipTypeCache[validPersonEntityRelType];
    }

    getDisclosureCount(typeCode, disclosureStatus) {
        if (disclosureStatus) {
            const VALUE = disclosureStatus.find(ele => Object.keys(ele) == typeCode);
            return VALUE ? VALUE[typeCode] : 0;
        }
    }

    openDefineRelationship(sfi, index) {
        this.selectedSfi = sfi;
        this.selectedSfiIndex = index;
        this.isShowSlider = true;
    }

    updateRelationshipStatus(hasDataChange = true) {
        if (this.disclosureId && hasDataChange) {
            this.$subscriptions.push(this.relationshipService.getSFIOfDisclosure(this.disclosureId)
                .subscribe((res: DisclosureSFIs) => {
                    setTimeout(() => {
                        this.disclosureSfi = res;
                        this.coiService.isRelationshipSaving = true;
                        this.conflictStatusChanged.next();
                    }, 500);
                }));
        }
    }
}
