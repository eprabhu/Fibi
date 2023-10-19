import { Component, Input } from '@angular/core';
import { DataService } from '../data.service';

@Component({
  standalone: true,
  selector: 'app-sponsor-card',
  template: `
      <div class="data-grid">
        <div class="mr-15">
            <img class="profile_img" [src]="imagePath" alt="an icon for representing a person">
        </div>
        <div class="ml-15 mr-8">
            <h2 class="heading">{{data.sponsor_name}}({{data.sponsor_code}})</h2>
        </div>
        <div class="align-items-end d-flex ms-auto">
          <button (click)="openSponsorDetails(data.sponsor_code)"
          class="align-items-center btn btn-primary d-flex fs-14"
          title="Click to view Sponsor Details"
          area-describedby="Click to view Sponsor Details">
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
export class SponsorCardComponent {

  @Input() data: any = {};
  @Input() imagePath: any = {};

  constructor(public graphDataService: DataService) {}

  openSponsorDetails(id) {
    this.graphDataService.openDetailsEvent.next({'node': 'Sponsor', 'id': id});
  }

}
