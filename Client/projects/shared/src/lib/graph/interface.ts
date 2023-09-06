export class GraphData {
    nodes: Array<any> = [];
    links: Array<any> = [];
}

export interface GraphEvent {
    index: number;
    clientX: number;
    clientY: number;
}

export class GraphDetail {
    id: string;
    visible: boolean;
}

export class GraphConfiguraion {
    nodes: any = {};
    relations: any = {};
}

export class GraphDataRO {
    node: string;
    value: string;
    relationship: Array<string>;
}
export const WIDTH = 1000;
export const HEIGHT = 800;
export const DISTANCE_BTN_NODES = 75;
export const FORCE_BTN_NODES = -500;

export const JSON_MAPPING = {
    101: './assets/graph/entity-graph.json'
};
