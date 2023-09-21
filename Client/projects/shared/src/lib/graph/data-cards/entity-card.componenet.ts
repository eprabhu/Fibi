import { Component, Input } from '@angular/core';

@Component({
  standalone: true,
  selector: 'app-entity-card',
  template: `
      <div class="data-grid">
        <div class="mr-15">
            <img class="profile_img" src="./assets/images/entity.png" alt="an Ican for represrnting a person">
        </div>
        <div class="ml-15">
            <h2 class="heading">{{data.name}}({{data.entity_number}})</h2>
            <p class="sub-heading">{{data.country_name}}</p>
            <p class="sub-heading">Type: {{data.type}}</p>
            <p class="sub-heading">Status: {{data.status}}</p>
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
      font-family: "Poppins";
      font-weight: 500;
      font-size: 18px;
      margin: 3px 0;
    }
    .sub-heading {
      font-family: "Avenir";
      font-size: 14px;
      color: rgba(34, 34, 34, 0.5);
      margin: 5px 0;
    }
  `],
})
export class EntityCardComponent {

  @Input() data: any = {};

}