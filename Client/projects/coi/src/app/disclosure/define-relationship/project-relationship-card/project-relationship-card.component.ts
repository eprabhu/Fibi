import { ChangeDetectionStrategy, Component, Input, OnDestroy, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { coiReviewComment, DisclosureProjectData } from '../../../shared-components/shared-interface';
import { CoiService } from '../../services/coi.service';
import { DataStoreService } from '../../services/data-store.service';

@Component({
    selector: 'app-project-relationship-card',
    templateUrl: './project-relationship-card.component.html',
    styleUrls: ['./project-relationship-card.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class ProjectRelationshipCardComponent implements OnInit, OnDestroy {

    @Input() projectDetails = new DisclosureProjectData();
    @Input() isCardExpanded = true;

    constructor(public commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService,) { }
    
    ngOnDestroy(): void {
        // console.log('destroy', this.projectDetails.title)
    }

    ngOnInit() {
        // console.log('create',this.projectDetails.title)
    }

    openReviewerComment(details, section, childSubSection) {
        let coiData = this._dataStore.getData();
        const disclosureDetails: coiReviewComment = {
            documentOwnerPersonId: coiData.coiDisclosure.person.personId,
            componentTypeCode: '6',
            subModuleItemKey: section === 'SFI' ? childSubSection?.disclosureDetailsId : details.moduleItemId,
            subModuleItemNumber: section === 'RELATIONSHIP' ? details.moduleCode : null,
            coiSubSectionsTitle: `#${details.projectNumber}: ${details.title}`,
            selectedProject: details,
            sfiStatus: childSubSection?.coiProjConflictStatusType,
            subSectionTitle: childSubSection?.personEntityRelationshipDto.entityName,
            subSectionId: childSubSection?.personEntityRelationshipDto.personEntityId,
        }
        this.commonService.$commentConfigurationDetails.next(disclosureDetails);
        this._coiService.isShowCommentNavBar = true;
    }

    collapse(): void {

    }

}
