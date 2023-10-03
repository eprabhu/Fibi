import { Component, Output, Input, OnInit, EventEmitter } from '@angular/core';
import { PersonDetailsModalService } from './person-details-modal.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-personal-details-modal',
    templateUrl: './personal-details-modal.component.html',
    styleUrls: ['./personal-details-modal.component.scss'],
    providers: [PersonDetailsModalService]
})
export class PersonalDetailsModalComponent implements OnInit {

    constructor(private _personservice: PersonDetailsModalService, private _commonservice: CommonService) { }

    @Input() userdetails: any;
    @Output() closeModalPersonDetails: EventEmitter<boolean> = new EventEmitter<boolean>();

    currentTab = 'PERSON_DETAILS';
    $subscriptions: Subscription[] = [];
    personValues: any;
    canShowPersonDetails = false;

    ngOnInit() {
        document.getElementById('entitypersondetailsTrigger').click();
        this.persondetails(this.userdetails);
        this.setPersonBtnRights();
    }

    addnewvalue(value): void {
        this.closeModalPersonDetails.emit(value);
    }

    persondetails(userDetails): void {
        this.$subscriptions.push(this._personservice.getPersonData(userDetails.personId).subscribe((data: any) => {
            this.personValues = data;
        },
        error => this._commonservice.showToast(HTTP_ERROR_STATUS, 'Fetching person details failed. Please try again.')));
    }

    viewPersoDetails(personTrainingId: string): void {
        const url = this._commonservice.fibiApplicationUrl + `#/fibi/person/person-details?personId=${personTrainingId}`;
        window.open(url);
    }

    setPersonBtnRights() {
        const isLoggedInPerson = this.userdetails.personId == this._commonservice.currentUserDetails.personId;
        this.canShowPersonDetails = (isLoggedInPerson || this._commonservice.getAvailableRight('MAINTAIN_PERSON'));
    }

}
