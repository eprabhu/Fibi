import { Component, Input } from '@angular/core';
import { DataService } from '../data.service';

@Component({
  standalone: true,
  selector: 'app-person-card',
  template: `
      <div class="data-grid">
        <div class="mr-15">
            <img class="profile_img" [src]="imagePath" alt="an icon for representing a person">
        </div>
        <div class="ml-15 mr-8">
            <h2 class="heading">{{data.full_name}}({{data.person_id}})</h2>
            <p class="sub-heading">Country : {{data.country_name}}</p>
            <p class="sub-heading">Username : {{data.user_name}}</p>
            <p class="sub-heading">Email : {{data.email_address}}</p>
            <p class="sub-heading">Designation : {{data.designation}}</p>
            <p class="sub-heading">Home Unit : {{data.home_unit}}</p>
        </div>
        <div class="align-items-end d-flex ms-auto">
        <button (click)="openEntity(data.person_id)"
        class="align-items-center btn btn-primary d-flex fs-14"
        title="Click to view Person Details"
        area-describedby="Click to view Person Details">
        <i class="fa fa-eye mr-2" aria-hidden="true"></i>
        View
        </button>
  </div>
      </div>
  `,
  styles: [
    `.profile_img {
      width: 50px;
      height: 50px;
      object-fit: cover;
      object-position: 50% 50%;
    }
    .data-grid {
      display: flex;
      flex-direction: row;
    }
    .heading {
      font-weight: bold;
      font-size: 18px;
      margin: 3px 0;
      color: #007dec;
    }
    .sub-heading {
      font-weight: bold;
      font-size: 14px;
      margin: 5px 0;
    }
  `],
})
export class PersonCardComponent {

  @Input() data: any = {};
  @Input() imagePath: any = {};

  constructor(public graphDataService: DataService) {}

  openEntity(id) {
    this.graphDataService.openDetailsEvent.next({'node': 'Person', 'id': id});
  }

}
