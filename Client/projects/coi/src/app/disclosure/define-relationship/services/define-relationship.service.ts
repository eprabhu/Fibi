import { Injectable } from '@angular/core';
import { COIModalConfig } from '../../../shared-components/coi-modal/coi-modal.interface';
import { AddConflictSlider, ApplyToAllModal, COI, CoiConflictStatusType, CoiDisclEntProjDetail, CoiProjConflictStatusType, ExpandCollapseSummaryBySection, FormattedConflictData, ProjectSfiRelationConflictRO, ProjectSfiRelationLoadRO, ProjectSfiRelations, RelationshipConflictType } from '../../coi-interface';
import { CommonService } from '../../../common/services/common.service';
import { HttpClient } from '@angular/common/http';
import { ScrollSpyConfiguration, ScrollSpyEvent } from '../../../shared-components/scroll-spy/scroll-spy.interface';
import { DataStoreService } from '../../services/data-store.service';
import { coiReviewComment } from '../../../shared-components/shared-interface';
import { CoiService } from '../../services/coi.service';
import { deepCloneObject } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

@Injectable()
export class DefineRelationshipService {

    searchText = '';
    isLoading = true;
    isEditMode = false;
    isShowErrorToast = false;
    searchKeys: Array<string> = [
        'piName',
        'projectId',
        'reporterRole',
        'projectStatus',
        '[projectNumber - title]',
        '[sponsorCode - sponsorName]',
        '[homeUnitNumber - homeUnitName]',
        '[primeSponsorCode - primeSponsorName]'
    ];
    isObserverActive: boolean[] = [];
    applyToAllModal = new ApplyToAllModal();
    isShowProjectSfiConflict: boolean[] = [];
    addConflictSlider = new AddConflictSlider();
    elementVisiblePercentageList: number[] = [];
    coiStatusList: CoiProjConflictStatusType[] = [];
    scrollSpyConfiguration = new ScrollSpyConfiguration();
    modalConfig = new COIModalConfig('coi-relation-modal', 'Apply to All', 'Cancel', 'lg');
    relationshipConflictType: RelationshipConflictType[] = [
        { statusCode: '1', projectConflictStatus: 'No Conflict', color: 'text-success', projectConflictStatusCode: '100' },
        { statusCode: '2', projectConflictStatus: 'Potential Conflict', color: 'text-warning', projectConflictStatusCode: '200' },
        { statusCode: '3', projectConflictStatus: 'Conflict Identified', color: 'text-danger', projectConflictStatusCode: '300' }
    ];

    constructor(private _http: HttpClient, private _commonService: CommonService, private _coiService: CoiService, private _dataStore: DataStoreService) { }

    updateObserverActivationStatus(totalCount: number, activeCounter: number, value: boolean) {
        for (let index = 0; index < totalCount; index++) {
            this.isObserverActive[index] = (activeCounter === index) ? true : value;
        }
    }

    configureScrollSpy(): void {
        this.scrollSpyConfiguration.activeCounter = 0;
        this.scrollSpyConfiguration.isActiveKeyNavigation = false;
        this.scrollSpyConfiguration.navItemClass = 'coi-scrollspy-right';
        this.scrollSpyConfiguration.contentItemClass = 'coi-scrollspy-left';
        this.setHeight();
    }

    updateScrollSpyConfig(event: { isVisible: boolean; observerEntry: IntersectionObserverEntry; }, SCROLL_SPY_INDEX: number): void {
        this.elementVisiblePercentageList[SCROLL_SPY_INDEX] = event.observerEntry.intersectionRatio;
        this.elementVisiblePercentageList = deepCloneObject(this.elementVisiblePercentageList);
    }

    setHeight(): void {
        const height = this.getDefineRelationShipNavHeight();
        this.scrollSpyConfiguration.scrollLeftHeight = height;
        this.scrollSpyConfiguration.activeCounter = 0;
        this.scrollSpyConfiguration.scrollRightHeight = height;
        this.scrollSpyConfiguration.rightOffsetTop = this.getNavOffsetTop();
        this.scrollSpyConfiguration.scrollRightHeight = height;
    }

    openReviewerComment(projectSfiRelation: ProjectSfiRelations, section: 'SFI' | 'RELATIONSHIP', childSubSection?: CoiDisclEntProjDetail) {
        const COI_DATA: COI = this._dataStore.getData();
        const disclosureDetails: coiReviewComment = {
            documentOwnerPersonId: COI_DATA.coiDisclosure.person.personId,
            componentTypeCode: '6',
            subModuleItemKey: section === 'SFI' ? childSubSection?.coiDisclProjectEntityRelId : projectSfiRelation?.moduleCode,
            subModuleItemNumber: section === 'RELATIONSHIP' ? projectSfiRelation?.moduleCode : null,
            coiSubSectionsTitle: `#${projectSfiRelation?.projectNumber}: ${projectSfiRelation?.title}`,
            selectedProject: projectSfiRelation,
            sfiStatus: childSubSection?.coiProjConflictStatusType,
            subSectionTitle: childSubSection?.personEntity?.entityName,
            subSectionId: childSubSection?.personEntity?.personEntityId,
        }
        this._commonService.$commentConfigurationDetails.next(disclosureDetails);
        this._coiService.isShowCommentNavBar = true;
    }

    getFormattedConflictData(coiDisclEntProjDetails: CoiDisclEntProjDetail[]): FormattedConflictData {
        const RESULT = coiDisclEntProjDetails?.reduce((acc, item) => {
            const CONFLICT_TYPE: RelationshipConflictType = deepCloneObject(this.relationshipConflictType.find(type => type.projectConflictStatusCode === item.projectConflictStatusCode));
            const { statusCode, projectConflictStatus } = CONFLICT_TYPE || {};
            if (statusCode) {
                acc.conflictCount[statusCode] = (acc.conflictCount[statusCode] || 0) + 1;
                acc.totalCount++;
                // Update the worst conflict status code if this statusCode is higher
                if (acc.conflictStatusCode === null || Number(statusCode) > Number(acc.conflictStatusCode)) {
                    acc.conflictStatusCode = statusCode;
                    acc.conflictStatus = projectConflictStatus;
                }
            }
            return acc;
        }, { 
            totalCount: 0,
            conflictStatus: null as string | null,
            conflictStatusCode: null as string | null,
            conflictCount: {} as { [key: string]: number }
        });
    
        return {
            conflictCount: RESULT.conflictCount,
            conflictStatus: RESULT.conflictStatus,
            conflictStatusCode: RESULT.conflictStatusCode,
            conflictCompleted: RESULT.totalCount === coiDisclEntProjDetails?.length
        };
    }

    updateDisclosureConflictStatus(disclConflictStatusType: CoiConflictStatusType): void {
        const COI_DATA = this._dataStore.getData();
        COI_DATA.coiDisclosure.coiConflictStatusType = disclConflictStatusType;
        COI_DATA.coiDisclosure.conflictStatusCode = disclConflictStatusType?.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'], { coiDisclosure: COI_DATA.coiDisclosure });
    }

    scrollSpyCounterChanged(event: ScrollSpyEvent): void {
        this._commonService.$globalEventNotifier.next({uniqueId: 'SCROLL_SPY', content: event});
    }

    resetElementVisiblePercentageList(): void {
        this.elementVisiblePercentageList = this.isEditMode ? [] : this.elementVisiblePercentageList.length > 3 ? this.elementVisiblePercentageList.slice(0, 2).concat(this.elementVisiblePercentageList.slice(-1)) : this.elementVisiblePercentageList;
        this.elementVisiblePercentageList = deepCloneObject(this.elementVisiblePercentageList);
    }

    clearAllServiceData(): void {
        this.searchText = '';
        this.isLoading = true;
        this.isEditMode = false;
        this.coiStatusList = [];
        this.isObserverActive = [];
        this.isShowErrorToast = false;
        this.isShowProjectSfiConflict = [];
        this.elementVisiblePercentageList = [];
        this._coiService.activeSectionId = 'COI801';
        this.applyToAllModal = new ApplyToAllModal();
        this.addConflictSlider = new AddConflictSlider();
        this.scrollSpyConfiguration = new ScrollSpyConfiguration();
        this._coiService.isExpandSummaryBySection = new ExpandCollapseSummaryBySection();
    }

    private getDefineRelationShipNavHeight(): string {
        const COI_DISCLOSURE_HEADER = document.getElementById('COI-DISCLOSURE-HEADER')?.getBoundingClientRect();
        const COI_FOOTER = document.getElementById('COI_FOOTER')?.getBoundingClientRect();
        const COI_DISCLOSURE_HEADER_BOTTOM = COI_DISCLOSURE_HEADER?.bottom;
        const PADDING = '2rem';
        const FOOTER_HEIGHT = COI_FOOTER.height;
        const TOTAL_HEIGHT = `${COI_DISCLOSURE_HEADER_BOTTOM}px - ${FOOTER_HEIGHT}px - ${PADDING}`
        return `calc(100vh - ${TOTAL_HEIGHT})`;
    }

    private getNavOffsetTop(): number {
        const element = document.getElementById('COI_DEFINE_RELATIONSHIP_NAV_HEADER');
        if (element) {
            const OFFSET_TOP = this.isEditMode ? 0 : 0;
            return element.getBoundingClientRect().height + 15 + OFFSET_TOP;
        }
        return 0;
    }

    lookups() {
        return this._http.get(`${this._commonService.baseUrl}/fcoiDisclosure/lookups`);
    }

    getProjectRelations(projectSfiRelationLoadRO: ProjectSfiRelationLoadRO) {
        return this._http.post(`${this._commonService.baseUrl}/fcoiDisclosure/project/relations`, projectSfiRelationLoadRO);
    }

    getSfiRelations(projectSfiRelationLoadRO: ProjectSfiRelationLoadRO) {
        return this._http.post(`${this._commonService.baseUrl}/fcoiDisclosure/entity/relations`, projectSfiRelationLoadRO);
    }

    saveProjectSfiConflict(projectSfiRelationConflictRO: ProjectSfiRelationConflictRO) {
        return this._http.post(`${this._commonService.baseUrl}/fcoiDisclosure/relation/conflict`, projectSfiRelationConflictRO);
    }

}
