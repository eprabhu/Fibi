import { Component, EventEmitter, HostListener, Input, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CountModalService } from './count-modal.service';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { getFormattedSponsor, getPersonLeadUnitDetails } from '../../common/utilities/custom-utilities';
import { RO } from '../../disclosure/coi-interface';
import { DisclosureProjectData } from '../../shared-components/shared-interface';
import { DISCLOSURE_CONFLICT_STATUS_BADGE } from '../../app-constants';

@Component({
    selector: 'app-count-modal',
    templateUrl: './count-modal.component.html',
    styleUrls: ['./count-modal.component.scss'],
    providers: [CountModalService]
})
export class CountModalComponent implements OnInit {

    @Input() moduleCode: number;
    @Input() disclosureId: number;
    @Input() disclosureSequenceStatusCode: number;
    @Input() disclosureNumber: any;
    @Input() inputType: any;
    @Input() personId: any;
    @Input() disclosureType: any;
    @Input() fcoiTypeCode: any;
    @Input() disfullData: any;
    @Input() projectType: any;
    @Input() disclosures: any;
    @Input() adminData: any;
    @Input() reviewerData: any;
    @Output() viewSlider: EventEmitter<any> = new EventEmitter<any>();
    @Output() closeModal: EventEmitter<boolean> = new EventEmitter<boolean>();

    currentModalTab: number;
    $subscriptions: Subscription[] = [];
    tableArray: any[] = [];
    private projectsList: DisclosureProjectData[] | any[] = [];
    coiFinancialEntityDetails: any[] = [];
    isEntityNameRead = false;
    isReadMore = [];

    constructor(private _countModalService: CountModalService, public commonService: CommonService) {
    }

    ngOnInit() {
        if (this.moduleCode === 8) {
            this.getSFIDatas();
        } else if (this.moduleCode === 101 && this.inputType === 'SFI_TAB') {
            this.getDisclosureDatas();
        } else {
            this.getProjectsList();
        }
    }

    getSFIDatas() {
        this.$subscriptions.push(this._countModalService
            .getSFICount(this.getRequestObject())
            .subscribe((data: any) => {
                this.coiFinancialEntityDetails = data.personEntities;
                document.getElementById('hidden-open-button').click();
            }));
    }

    getRequestObject() {
		const REQ_OBJ = new RO();
        REQ_OBJ.currentPage = 0;
        REQ_OBJ.disclosureId = this.disclosureId;
        REQ_OBJ.filterType = '';
        REQ_OBJ.pageNumber = 0;
        REQ_OBJ.personId = this.personId;
        REQ_OBJ.reviewStatusCode = '';
        REQ_OBJ.searchWord = '';
        return REQ_OBJ;
    }

    getProjectsList() {
        if (this.inputType === 'DISCLOSURE_TAB') {
            this.$subscriptions.push(this._countModalService
                .getDisclosureProjects(this.disclosureId)
                .subscribe((data: any) => {
                    this.projectsList = this.getUpdatedProjectsList(data);
                    this.switchTableData(this.moduleCode);
                    document.getElementById('hidden-open-button').click();
                }, err => {
                    this.closeCountModal();
                }));
        }
    }

    getDisclosureDatas() {
        this.$subscriptions.push(this._countModalService.getDisclosureDetails(this.disclosureId).subscribe((data: any) => {
            this.coiFinancialEntityDetails = data;
            document.getElementById('hidden-open-button').click();
        }));
    }

    modalHeader() {
        if (this.fcoiTypeCode == 1) {
            if (this.moduleCode == 8) {
                return `SFIs Attached to : FCOI Disclosure By ${this.getFcoiFullName()} [ ${this.getFcoiUnitName()} ]`;
            } else if (this.moduleCode == 1 || this.moduleCode == 3) {
                return `Projects Related to : FCOI Disclosure By ${this.getFcoiFullName()} [ ${this.getFcoiUnitName()} ]`;
            }
        } else {
            if (this.fcoiTypeCode == 2 || this.fcoiTypeCode == 3) {
                if (this.moduleCode == 8) {
                    return `SFIs Attached to : ${this.getType()} Disclosure - [ ${this.getTitle()} ] By ${this.getFullName()}`;
                } else if (this.moduleCode == 1 || this.moduleCode == 3) {
                    return `Projects Related to ${this.getTitle()} `;
                }
            }
        }
    }

    // getType
    getType() {
        if (this.disclosures?.projectType) {
            return this.disclosures?.projectType;
        } else if (this.disfullData?.coiDisclosure?.coiDisclosureFcoiType?.description) {
            return this.disfullData?.coiDisclosure?.coiDisclosureFcoiType?.description;
        } else if (this.adminData?.projectType) {
            return this.adminData?.projectType;
        } else if (this.reviewerData?.fcoiType) {
            return this.reviewerData?.fcoiType;
        }
    }

    // title
    getTitle(): string {
        if (this.disclosures?.fcoiTypeCode === '2') {
            return this.reduceTitleLength(this.disclosures?.projectHeader);
        } else if (this.disfullData?.projectDetail?.title) {
            return this.reduceTitleLength(this.disfullData?.projectDetail?.title);
        } else if (this.adminData?.fcoiTypeCode === '2') {
            return this.reduceTitleLength(this.adminData?.projectHeader);
        } else if (this.reviewerData?.fcoiTypeCode) {
            if (this.reviewerData?.fcoiTypeCode === '3') {
                return this.reduceTitleLength(this.reviewerData.awardTitle);
            } else {
                return this.reduceTitleLength(this.reviewerData?.proposalTitle);
            }
        }
    }

    reduceTitleLength(title: string): string {
        return title?.length > 50 ? title?.slice(0, 50) + '...' : title;
    }

    // FullName
    getFullName(): string {
        if (this.disclosures?.disclosurePersonFullName) {
            return this.disclosures?.disclosurePersonFullName;
        } else if (this.disfullData?.coiDisclosure?.person?.fullName) {
            return this.disfullData?.coiDisclosure?.person?.fullName;
        } else if (this.adminData?.disclosurePersonFullName) {
            return this.adminData?.disclosurePersonFullName;
        } else if (this.reviewerData?.disclosurePersonFullName) {
            return this.reviewerData?.disclosurePersonFullName;
        }
    }

    // FCOI FullName
    getFcoiFullName(): string {
        if (this.disfullData?.coiDisclosure?.person?.fullName) {
            return this.disfullData?.coiDisclosure?.person?.fullName;
        } else if (this.adminData?.disclosurePersonFullName) {
            return this.adminData?.disclosurePersonFullName;
        } else if (this.disclosures?.disclosurePersonFullName) {
            return this.disclosures?.disclosurePersonFullName;
        }
    }

    // FCOI Unit Name
    getFcoiUnitName() {
        if (this.disfullData?.coiDisclosure?.person?.unit) {
            return getPersonLeadUnitDetails(this.disfullData?.coiDisclosure?.person?.unit);
        } else if (this.adminData?.unit) {
            return getPersonLeadUnitDetails(this.adminData?.unit);
        } else if (this.disclosures?.unit) {
            return getPersonLeadUnitDetails(this.disclosures?.unit);
        }
    }

    closeCountModal() {
        hideModal('coiCountsViewModal');
        setTimeout(() => {
            this.closeModal.emit(false);
        }, 200);
    }

    switchTableData(moduleCode: number) {
        this.currentModalTab = moduleCode;
        this.tableArray = this.filterByModuleCode(moduleCode);
        this.isReadMore = [];
        document.getElementById('project-count-modal-tabele-scroll')?.scroll(0,0);
    }

    private filterByModuleCode(moduleCode: number): any[] {
        return this.projectsList.filter(item => item.moduleCode === moduleCode);
    }

    private getUpdatedProjectsList(projectsList: any[]): any[] {
        return projectsList.map((project: any) => {
            return {
                ...project,
                formattedLeadUnit: this.commonService.getPersonLeadUnitDetails(project),
                formattedSponsor: getFormattedSponsor(project?.sponsorCode, project?.sponsorName),
                formattedPrimeSponsor: getFormattedSponsor(project?.primeSponsorCode, project?.primeSponsorName),
                disclosureConflictBadge: DISCLOSURE_CONFLICT_STATUS_BADGE[project?.conflictStatusCode]
            };
        });
    }
    

    viewSliderEmit(flag: boolean, entityId) {
        this.viewSlider.emit({'flag': flag, 'entityId': entityId})
    }

    showFullTitle(): string {
        if (this.disclosures?.fcoiTypeCode === '2') {
            return this.disclosures?.projectHeader;
        } else if (this.disfullData?.projectDetail?.title) {
            return this.disfullData?.projectDetail?.title;
        } else if (this.adminData?.fcoiTypeCode) {
            return this.adminData?.projectHeader;
        } else if (this.reviewerData?.fcoiTypeCode) {
            if (this.reviewerData?.fcoiTypeCode === '3') {
                return this.reviewerData.awardTitle;
            } else {
                return this.reviewerData?.proposalTitle;
            }
        }
    }

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            this.closeModal.emit(false);
        }
    }
}
