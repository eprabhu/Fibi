import {Component, Input, OnInit} from '@angular/core';
import {Router} from '@angular/router';
import {ElasticConfigService} from 'projects/fibi/src/app/common/services/elastic-config.service';
import {Subscription} from 'rxjs';
import {
    getEndPointOptionsForAwardNumber,
    getEndPointOptionsForDepartment,
    getEndPointOptionsForProposalDisclosure,
    getEndPointOptionsForSponsor
} from '../../../../../fibi/src/app/common/services/end-point.config';
import {hideModal, openModal} from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import {CoiService} from '../services/coi.service';
import {DataStoreService} from '../services/data-store.service';
import {UserDashboardService} from "../../user-dashboard/user-dashboard.service";
import {CommonService} from "../../common/services/common.service";

declare var $: any;

@Component({
    selector: 'app-project-disclosure',
    templateUrl: './project-disclosure.component.html',
    styleUrls: ['./project-disclosure.component.scss']
})
export class ProjectDisclosureComponent implements OnInit {

    isShowResultCard = false;
    clearProjectField: String;
    clearSponsorField: String;
    clearPIField: String;
    clearLUField: String;
    manualProjectAddDetails: any = {};
    projectDisclosureValidation = new Map();
    projectSearchOptions: any;
    isSearchExternalProject = false;
    $subscriptions: Subscription[] = [];
    piElasticSearchOptions: any = {};
    unitHttpOptions: any = {};
    sponsorSearchOptions: any = {};
    projectTypes = [];
    selectedProjectType = null;

    constructor(public coiService: CoiService, public _dataStore: DataStoreService,
                private _router: Router, private _commonService: CommonService,
                private _elasticConfig: ElasticConfigService, private _userDashboardService: UserDashboardService) {
    }

    ngOnInit() {
        this.getCoiProjectTypes();
        this.changeProjectType();
        this.piElasticSearchOptions = this._elasticConfig.getElasticForPerson();
        this.unitHttpOptions = getEndPointOptionsForDepartment();
        this.sponsorSearchOptions = getEndPointOptionsForSponsor();

    }

    clearDisclosureModal() {
        hideModal('createProjectDisclosureModal');
        setTimeout(() => this._userDashboardService.isModalOpen = false);
        this.isShowResultCard = false;
        this.clearProjectField = new String('true');
        this.clearSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.manualProjectAddDetails = {};
        this.manualProjectAddDetails.moduleItemId = null;
        this.manualProjectAddDetails.title = null;
        this.projectDisclosureValidation.clear();

    }

    selectedProposal(event) {
        if (event) {
            this.manualProjectAddDetails.moduleItemId = this.selectedProjectType == 'Award' ? event.awardId: event.moduleItemId;
            this.isShowResultCard = true;
        } else {
            this.clearSearchFields();
        }
    }

    clearSearchFields() {

        this.changeProjectType();

    }

    createProjectDisclosureAPI() {
        if (this.validateProject()) {
            this.$subscriptions.push(this.coiService.createDisclosure({coiDisclosure:{
                    coiProjectTypeCode: this.getCoiProjectTypeFromCode(),
                    moduleItemKey: this.manualProjectAddDetails.moduleItemId,
                    personId: this._commonService.getCurrentUserDetail('personID')
            }}).subscribe((data: any) => {
                if (data) {
                    this._router.navigate(['/coi/create-disclosure/screening'], {
                        queryParams: {
                            disclosureId: data.coiDisclosure.disclosureId
                        }
                    });
                    this.clearDisclosureModal();
                }
            }, _err => {
            }));
        }
    }

    getCoiProjectTypeFromCode(description = this.selectedProjectType) {
        return this.projectTypes.find(type => type.description === description).coiProjectTypeCode;
    }

    validateProject() {
        this.projectDisclosureValidation.clear();
        if(!this.selectedProjectType) {
            this.projectDisclosureValidation.set('projectSelect', 'Please select any one of the given Project Type');
        }
        if (!this.manualProjectAddDetails || !this.manualProjectAddDetails.moduleItemId) {
            this.projectDisclosureValidation.set('proposalSearch', 'Please select a '+ this.selectedProjectType +' to create disclosure.');
        }
        return this.projectDisclosureValidation.size === 0 ? true : false;
    }

    changeProjectType() {
        this.clearProjectField = new String('true');
        this.isShowResultCard = false;
        this.clearSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.manualProjectAddDetails = {};
        this.manualProjectAddDetails.moduleItemId = null;
        this.manualProjectAddDetails.title = null;
        this.projectDisclosureValidation.clear();
        switch (this.selectedProjectType) {
            case 'Award':
                return this.projectSearchOptions = getEndPointOptionsForAwardNumber();
            case 'Development Proposal':
                return this.projectSearchOptions = getEndPointOptionsForProposalDisclosure();
        }
    }

    resetManualProjectAddFields() {
        this.clearSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.manualProjectAddDetails = {};
        this.manualProjectAddDetails.moduleItemId = null;
        this.manualProjectAddDetails.title = null;
    }

    switchExternalProject(isExternal: boolean) {
        this.isSearchExternalProject = isExternal;
        this.resetManualProjectAddFields();
        this.clearProjectField = new String('true');
        this.changeProjectType();
        this.isShowResultCard = false;
    }

    getCoiProjectTypes() {
        this.$subscriptions.push(this.coiService.getCoiProjectTypes().subscribe((res: any) => {
            this.projectTypes = res.coiProjectTypes;
            openModal('createProjectDisclosureModal');
        }));
    }

}
