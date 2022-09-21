import { Component, OnDestroy, OnInit } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { CertificationService } from './certification.service';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { NotificationModalConfig, PersonNotificationMailLog, ProposalPerson, UpdateCertificateRO } from '../interface/proposal.interface';
import { ProposalService } from '../services/proposal.service';
import { WebSocketService } from '../../common/services/web-socket.service';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { DataStoreService } from '../services/data-store.service';
import {concatUnitNumberAndUnitName} from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-certification',
    templateUrl: './certification.component.html',
    styleUrls: ['./certification.component.css'],
    providers: [CertificationService]
})
export class CertificationComponent implements OnInit, OnDestroy {

    proposalId: number;
    proposalPersons: ProposalPerson[] = [];
    selectedPerson: ProposalPerson;
    selectedModalPerson: ProposalPerson;
    configuration = {
        moduleItemCode: 3,
        moduleSubitemCodes: [],
        moduleItemKey: null,
        moduleSubItemKey: '',
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: true,
        isEnableVersion: true,
        isChangeWarning: true
    };
    bellIconHover: boolean[] = [];
    notifications: PersonNotificationMailLog[] = [];
    loggedInPersonId = this._commonService.getCurrentUserDetail('personID');
    dataDependencies = ['proposal', 'dataVisibilityObj', 'availableRights', 'proposalPersons'];
    dataVisibilityObj: any;
    result: any = {};
    currentMode = '';
    $subscriptions: Subscription[] = [];
    concatUnitNumberAndUnitName = concatUnitNumberAndUnitName;

    constructor(
        private _commonService: CommonService,
        private _certificationService: CertificationService,
        public _proposalService: ProposalService,
        private _route: ActivatedRoute,
        private _router: Router,
        private _websocket: WebSocketService,
		private _dataStore: DataStoreService,
        public _autoSaveService: AutoSaveService
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.currentMode = this.dataVisibilityObj.mode;
        this.getProposalCertificates();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
        this.setUnsavedChanges(false);
    }

    private getDataFromStore() {
		this.result = this._dataStore.getData(this.dataDependencies);
        this.dataVisibilityObj = this.result.dataVisibilityObj;
        this.proposalId = this.result.proposal.proposalId;
        if (this.selectedPerson && this.currentMode !== this.dataVisibilityObj.mode) {
            this.currentMode = this.dataVisibilityObj.mode;
            this.configureSelectedPersonDetails();
        }
	}

	private listenDataChangeFromStore() {
		this.$subscriptions.push(
			this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
				if (dependencies.some((dep) => this.dataDependencies.includes(dep))) {
					this.getDataFromStore();
				}
			})
		);
	}

    getProposalCertificates() {
        this.$subscriptions.push(this._certificationService
            .getProposalPersonsForCertification(
                {proposalId: this.proposalId,
                proposalEditable: this._proposalService.proposalMode === 'edit'
            }).subscribe((res: { proposalPersons: ProposalPerson[] }) => {
                this.proposalPersons = res.proposalPersons;
                this.selectPersonPerson();
            }));
    }

    selectPerson(person: ProposalPerson): void {
        if (this.selectedPerson.proposalPersonId !== person.proposalPersonId) {
            this.selectedPerson = person;
            this.configureSelectedPersonDetails();
        }
    }

    updatePersonCertificationStatus(saveEvent: any) {
        if ((this.selectedPerson.personCertified && !saveEvent.IS_ALL_QUESTIONNAIRES_COMPLETE) ||
            (!this.selectedPerson.personCertified && saveEvent.IS_ALL_QUESTIONNAIRES_COMPLETE)) {
            const RO = this.generateUpdateCertificateRO(saveEvent.IS_ALL_QUESTIONNAIRES_COMPLETE);
            this.$subscriptions.push(this._certificationService
                .updateProposalPersonCertification(RO)
                .subscribe((_res: any) => {
                    this.selectedPerson.personCertified = RO.status;
                    this.updateCertificationGlobally();
                }));
        }
    }

    generateModalConfig(mode, selectedPerson = null): NotificationModalConfig {
        return { mode, selectedPerson };
    }

    showNotificationLog(person: ProposalPerson) {
        this.selectedModalPerson = JSON.parse(JSON.stringify(person));
        this._proposalService.notifyModalData.next(this.generateModalConfig('SINGLE', this.selectedModalPerson));
    }

    notifyAllPersons() {
        this._proposalService.notifyModalData.next(this.generateModalConfig('ALL'));
        this.selectedModalPerson = null;
    }

    /**
     * certification screen should select a person if the person id is in params, if not then check if logged in user is present in the list
     * or else select the first person in the person list.
     */
    private getSelectedPersonForFirstLoad(): ProposalPerson {
        return this.getPersonFromParams() || this.getLoggedInUser() || this.proposalPersons[0];
    }

    private getPersonFromParams(): ProposalPerson {
        const selectedProposalPersonId = Number(this._route.snapshot.queryParams['proposalPersonId']);
        if (!selectedProposalPersonId) { return undefined; }
        this._router.navigate([], { queryParams: { proposalPersonId: null }, queryParamsHandling: 'merge' });
        return this.getPersonFromProposalPersonId(selectedProposalPersonId);
    }

    private getLoggedInUser(): ProposalPerson {
        return this.getPersonFromPersonID(this._commonService.getCurrentUserDetail('personID'));
    }

    private getPersonFromProposalPersonId(proposalPersonId: number): ProposalPerson {
        return this.findFromProposalPerson(this.compareProposalPersonId(proposalPersonId));
    }

    private getPersonFromPersonID(personID: string): ProposalPerson {
        return this.findFromProposalPerson(this.comparePersonId(personID));
    }

    private findFromProposalPerson(matchingPropertyFunction: (person: ProposalPerson) => boolean): ProposalPerson {
        return this.proposalPersons.find(matchingPropertyFunction);
    }

    private comparePersonId(personID: string): (person: ProposalPerson) => boolean {
        return (person: ProposalPerson) => person.personId === personID;
    }

    private compareProposalPersonId(proposalPersonId): (person: ProposalPerson) => boolean {
        return (person: ProposalPerson) => person.proposalPersonId === proposalPersonId;
    }

    private selectPersonPerson() {
        if (this.proposalPersons.length) {
            this.selectedPerson = this.getSelectedPersonForFirstLoad();
            this.configureSelectedPersonDetails();
            this.configuration.moduleItemKey = this.proposalId;
        }
    }

    private configureSelectedPersonDetails() {
        this.configuration.moduleSubitemCodes = [3];
        this.configuration.moduleSubItemKey = this.getSelectedPersonId().personId;
        this.configuration.enableViewMode = !this.isQuestionnaireEditMode();
        this.configuration = { ...this.configuration };
    }

    private isQuestionnaireEditMode(): boolean {
        const isLoggedInUser = this.loggedInPersonId === this.selectedPerson.personId;
        const hasProxyCertificationRight = this.result.availableRights.includes('PROXY_PROPOSAL_PERSON_CERTIFICATION');
        return [3, 1, 9, 12].includes(this.result.proposal.statusCode) && (isLoggedInUser || hasProxyCertificationRight);
    }

    private getSelectedPersonId(): { personId: string, isNonEmployee: boolean, proposalPersonRole: number } {
        return {
            personId: this.selectedPerson.personId ? this.selectedPerson.personId : this.selectedPerson.rolodexId,
            isNonEmployee: Boolean(this.selectedPerson.rolodexId),
            proposalPersonRole: this.selectedPerson.proposalPersonRole.id
        };
    }

    private generateUpdateCertificateRO(questionnaireCompleted: boolean): UpdateCertificateRO {
        const { personId, isNonEmployee, proposalPersonRole } = this.getSelectedPersonId();
        return {
            personId,
            proposalId: this.proposalId,
            isNonEmployee,
            status: questionnaireCompleted,
            proposalPersonRole
        };
    }

    private updateCertificationGlobally() {
        this.result.proposalPersons.find((person: ProposalPerson) => person.proposalPersonId === this.selectedPerson.proposalPersonId)
            .personCertified = this.selectedPerson.personCertified;
        this._dataStore.updateStore(['proposalPersons'], this.result);
    }

    markQuestionnaireAsEdited(changeStatus: boolean): void {
        this.setUnsavedChanges(changeStatus);
    }

    setUnsavedChanges(flag: boolean) {
        if (this.dataVisibilityObj.dataChangeFlag !== flag) {
            this._autoSaveService.setUnsavedChanges('Certification', 'proposal-certification', flag, true);
            this.dataVisibilityObj.dataChangeFlag = flag;
            this._dataStore.updateStore(['dataVisibilityObj'], this);
        }
    }
}
