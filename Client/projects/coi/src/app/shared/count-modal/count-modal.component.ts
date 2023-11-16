import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CountModalService } from './count-modal.service';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { getSponsorSearchDefaultValue } from '../../common/utilities/custom-utilities';
import { RO } from '../../disclosure/coi-interface';

@Component({
    selector: 'app-count-modal',
    templateUrl: './count-modal.component.html',
    styleUrls: ['./count-modal.component.css'],
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
    $subscriptions: Subscription[] = [];
    tableArray: any[] = [];
    currentModalTab = 'Award';
    projectDatas: any;
    coiFinancialEntityDetails: any[] = [];
    isEntityNameRead = false;

    constructor(private _countModalService: CountModalService, private _commonService: CommonService) {
    }

    ngOnInit() {
        if (this.moduleCode === 8) {
            this.getSFIDatas();
        } else if (this.moduleCode === 101 && this.inputType === 'SFI_TAB') {
            this.getDisclosureDatas();
        } else {
            this.getProjectDatas();
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

    getProjectDatas() {
        if (this.inputType === 'DISCLOSURE_TAB') {
            this.$subscriptions.push(this._countModalService
                .getProjectsCount(this.disclosureId, this.disclosureSequenceStatusCode, this.personId)
                .subscribe((data: any) => {
                    this.projectDatas = data;
                    this.currentModalTab = this.moduleCode === 1 ? 'Award' : 'Proposal';
                    this.switchTableData();
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
                return `SFIs Attached to : FCOI Disclosure By ${this.getFcoiFullName()} [ Unit : ${this.getFcoiUnitName()} ]`;
            } else if (this.moduleCode == 1 || this.moduleCode == 3) {
                return `Projects Related to : FCOI Disclosure By ${this.getFcoiFullName()} [ Unit : ${this.getFcoiUnitName()} ]`;
            }
        } else {
            if (this.fcoiTypeCode == 2 || this.fcoiTypeCode == 3) {
                if (this.moduleCode == 8) {
                    return `SFIs Attached to : ${this.getType()} Disclosure For [ ${this.gettitle()} ] By ${this.getFullName()}`;
                } else if (this.moduleCode == 1 || this.moduleCode == 3) {
                    return `Projects Related to ${this.gettitle()} `;
                }
            }
        }
    }

    // getType
    getType() {
        if (this.disclosures?.fcoiType) {
            return this.disclosures?.fcoiType;
        } else if (this.disfullData?.coiDisclosure?.coiDisclosureFcoiType?.description) {
            return this.disfullData?.coiDisclosure?.coiDisclosureFcoiType?.description;
        } else if (this.adminData?.fcoiType) {
            return this.adminData?.fcoiType;
        } else if (this.reviewerData?.fcoiType) {
            return this.reviewerData?.fcoiType;
        }
    }

    // title
    gettitle(): string {
        if (this.disclosures?.fcoiType) {
            if (this.disclosures?.fcoiType === 'Proposal') {
                return this.reduceTitleLength(this.disclosures?.proposalTitle);
            } else {
                return this.reduceTitleLength(this.disclosures?.awardTitle);
            }
        } else if (this.disfullData?.projectDetail?.title) {
            return this.reduceTitleLength(this.disfullData?.projectDetail?.title);
        } else if (this.adminData?.fcoiType) {
            if (this.adminData?.fcoiType === 'Award') {
                return this.reduceTitleLength(this.adminData.awardTitle);
            } else {
                return this.reduceTitleLength(this.adminData.proposalTitle);
            }
        } else if (this.reviewerData?.fcoiType) {
            if (this.reviewerData?.fcoiType === 'Award') {
                return this.reduceTitleLength(this.reviewerData.awardTitle);
            } else {
                return this.reduceTitleLength(this.reviewerData?.proposalTitle);
            }
        }
    }

    reduceTitleLength(title: string): string {
        return title.length > 90 ? title.slice(0, 90) + '...' : title;
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
            return getSponsorSearchDefaultValue(this.disfullData?.coiDisclosure?.person?.unit);
        } else if (this.adminData?.unit) {
            return getSponsorSearchDefaultValue(this.adminData?.unit);
        } else if (this.disclosures?.unit) {
            return getSponsorSearchDefaultValue(this.disclosures?.unit);
        }
    }

    closeCountModal() {
        hideModal('coiCountsViewModal');
        this.closeModal.emit(false);
    }

    switchTableData() {
        this.tableArray = this.currentModalTab === 'Proposal' ? this.projectDatas.proposals : this.projectDatas.awards;
    }


    openProjectMoreDetails(currentModalTab, moduleId) {
        if (currentModalTab === 'Proposal') {
            const proposalLink = this._commonService.fibiApplicationUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
            window.open(proposalLink, '_blank');
        } else {
            const awardLink = this._commonService.fibiApplicationUrl + '#/fibi/award/overview?awardId=' + moduleId;
            window.open(awardLink, '_blank');
        }
    }

    viewSliderEmit(flag: boolean, entityId) {
        this.viewSlider.emit({'flag': flag, 'entityId': entityId})
    }


}
