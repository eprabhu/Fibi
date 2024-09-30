import { Subscription } from 'rxjs';
import { RO } from '../../disclosure/coi-interface';
import { CoiCountModalService } from './coi-count-modal.service';
import { CommonService } from '../../common/services/common.service';
import { DashboardProjectCount } from '../../common/services/coi-common.interface';
import { COIModalConfig, ModalActionEvent } from '../coi-modal/coi-modal.interface';
import { PROJECT_CONFLICT_STATUS_BADGE, HTTP_ERROR_STATUS, PROJECT_TYPE } from '../../app-constants';
import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { closeCommonModal, getFormattedSponsor, openCommonModal } from '../../common/utilities/custom-utilities';
import { COICountModalViewSlider, COICountModal, COICountModalClose, CountModalDisclosureProjectData, COICountModalProjectUpdate } from '../shared-interface';

@Component({
    selector: 'app-coi-count-modal',
    templateUrl: './coi-count-modal.component.html',
    styleUrls: ['./coi-count-modal.component.scss'],
    providers: [CoiCountModalService]
})
export class CoiCountModalComponent implements OnInit, OnDestroy {

    slicedHeaderPart = '';
    SfiDetailsList: any[] = [];
    PROJECT_TYPE = PROJECT_TYPE;
    currentActiveModuleCode: number;
    $subscriptions: Subscription[] = [];
    COI_COUNT_MODAL_ID = 'coi-count-modal';
    projectCountList: DashboardProjectCount[] = [];
    projectsList: CountModalDisclosureProjectData[] = [];
    filteredProjectsList: CountModalDisclosureProjectData[] = [];
    countModalConfig = new COIModalConfig(this.COI_COUNT_MODAL_ID, '', 'Close', 'xl');

    @Input() coiCountModal = new COICountModal();

    @Output() viewSlider = new EventEmitter<COICountModalViewSlider>();
    @Output() countModalClose = new EventEmitter<COICountModalClose>();
    @Output() coiCountModalChange = new EventEmitter<COICountModal>();

    constructor(private _coiCountModalService: CoiCountModalService, public commonService: CommonService) {}

    ngOnInit(): void {
        const { fcoiTypeCode, moduleCode, inputType } = this.coiCountModal;
        this.slicedHeaderPart = fcoiTypeCode !== '2' ? this.getFormattedPeronUnit() : this.getProjectHeader();
        if (moduleCode === 8) {
            this.getSFIDatas();
        } else if (moduleCode === 101 && inputType === 'SFI_TAB') {
            this.getDisclosureDatas();
        } else if (inputType === 'DISCLOSURE_TAB') {
            this.getProjectsList();
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getRequestObject(): RO {
        return {
            pageNumber: 0,
            currentPage: 0,
            filterType: '',
            searchWord: '',
            reviewStatusCode: '',
            dispositionStatusCode: null,
            personId: this.coiCountModal.personId,
            disclosureId: this.coiCountModal.disclosureId
        };
    }

    private getSFIDatas(): void {
        this.$subscriptions.push(
            this._coiCountModalService.getSFICount(this.getRequestObject())
                .subscribe((data: any) => {
                    this.SfiDetailsList = data.personEntities;
                    openCommonModal(this.COI_COUNT_MODAL_ID);
                }, (_error: any) => {
                    this.closeCoiCountModal();
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                }));
    }

    private getProjectsList(): void {
        this.$subscriptions.push(
            this._coiCountModalService.getDisclosureProjects(this.coiCountModal.disclosureId)
                .subscribe((projectList: any) => {
                    this.setProjectList(projectList);
                    openCommonModal(this.COI_COUNT_MODAL_ID);
                }, (_error: any) => {
                    this.closeCoiCountModal();
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                }));
    }

    private setProjectList(projectList: any): void {
        const UPDATED_OBJECT: COICountModalProjectUpdate = this.getUpdatedProjectsList(projectList);
        this.projectsList = UPDATED_OBJECT.updatedProjectsList;
        this.projectCountList = UPDATED_OBJECT.projectCountList;
        this.switchProject(this.coiCountModal.moduleCode);
    }

    private getUpdatedProjectsList(projectsList: CountModalDisclosureProjectData[]): COICountModalProjectUpdate {
        const PROJECT_COUNT_MAP = new Map<number, DashboardProjectCount>();
        const UPDATED_PROJECT_LIST = projectsList.map((project: CountModalDisclosureProjectData) => {
            const { moduleCode, projectType, sponsorCode, sponsorName, primeSponsorCode, primeSponsorName, projectConflictStatusCode } = project;
            // Update the project with formatted fields
            const UPDATED_PROJECT = {
                ...project,
                formattedSponsor: getFormattedSponsor(sponsorCode, sponsorName),
                formattedProjectHeader: this.getFormattedProjectHeader(project),
                formattedLeadUnit: this.commonService.getPersonLeadUnitDetails(project),
                disclosureConflictBadge: PROJECT_CONFLICT_STATUS_BADGE[projectConflictStatusCode],
                formattedPrimeSponsor: getFormattedSponsor(primeSponsorCode, primeSponsorName)
            };
            // Update project count map
            if (moduleCode >= 0 && projectType) {
                const PROJECT_COUNT = PROJECT_COUNT_MAP.get(moduleCode) || { moduleCode, projectType, projectCount: 0 };
                PROJECT_COUNT.projectCount++;
                PROJECT_COUNT_MAP.set(moduleCode, PROJECT_COUNT);
            }
            return UPDATED_PROJECT;
        });

        return {
            updatedProjectsList: UPDATED_PROJECT_LIST,
            projectCountList: Array.from(PROJECT_COUNT_MAP.values())
        };
    }

    private getFormattedProjectHeader(project: CountModalDisclosureProjectData): string {
        const { projectNumber, title } = project || {};
        if (projectNumber && title) {
            return `#${projectNumber} - ${title}`;
        }
        return projectNumber ? `#${projectNumber}` : title || '';
    }
    

    private getDisclosureDatas(): void {
        this.$subscriptions.push(
            this._coiCountModalService.getDisclosureDetails(this.coiCountModal.disclosureId)
                .subscribe((data: any) => {
                    this.SfiDetailsList = data;
                    openCommonModal(this.COI_COUNT_MODAL_ID);
                }, (_error: any) => {
                    this.closeCoiCountModal();
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                }));
    }

    private filterByModuleCode(moduleCode: number): CountModalDisclosureProjectData[] {
        return this.projectsList.filter((item: CountModalDisclosureProjectData) => item.moduleCode === moduleCode);
    }

    switchProject(moduleCode: number): void {
        this.currentActiveModuleCode = moduleCode;
        this.filteredProjectsList = this.filterByModuleCode(moduleCode);
        document.getElementById('project-count-modal-tabele-scroll')?.scroll(0, 0);
    }

    getFormattedPeronUnit(): string {
        return this.commonService.getPersonLeadUnitDetails(this.coiCountModal.personUnit);
    }

    getProjectHeader(): string {
        return this.coiCountModal.projectHeader || `#${this.coiCountModal.projectNumber} - ${this.coiCountModal.projectTitle}`;
    }

    coiCountModalAction(event: ModalActionEvent): void {
        switch (event.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN': this.closeCoiCountModal();
        }
    }

    closeCoiCountModal(): void {
        closeCommonModal(this.COI_COUNT_MODAL_ID);
        setTimeout(() => {
            this.coiCountModal = new COICountModal();
            this.coiCountModalChange.emit(this.coiCountModal);
            this.countModalClose.emit({ isOpenCountModal: false });
        }, 200);
    }

    viewSliderEmit(isOpenSlider: boolean, entityId: number | string): void {
        this.viewSlider.emit({ isOpenSlider, entityId });
    }

    redirectToProjectDetails(projectDetails: CountModalDisclosureProjectData): void {
        const { documentNumber, projectId, projectTypeCode } = projectDetails;
        this.commonService.redirectToProjectDetails(projectTypeCode, (documentNumber || projectId));
    }

}
