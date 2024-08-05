import { JsonDecoder } from 'ts.data.json';
import { Name, nameDecoder } from '@/arkham/types/Name';

type CardCost = { contents: number, tag: "StaticCost" } | { tag: "DynamicCost" } | { tag: "DiscardAmountCost" }

type SkillIcon = { contents: string, tag: "SkillIcon" } | { tag: "WildIcon" } | { tag: "WildMinusIcon" }


export type CardDef = {
  cardCode: string;
  classSymbols: string[];
  cardType: string;
  art: string;
  level: number | null;
  name: Name;
  cardTraits: string[];
  skills: SkillIcon[];
  cost: CardCost | null;
}

const cardCostDecoder = JsonDecoder.oneOf<CardCost>([
  JsonDecoder.object({ contents: JsonDecoder.number, tag: JsonDecoder.isExactly("StaticCost") }, 'StaticCost'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("DynamicCost") }, 'DynamicCost'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("MaxDynamicCost") }, 'MaxDynamicCost').map(() => ({ tag: "DynamicCost"})),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("DiscardAmountCost") }, 'DiscardAmountCost')
], 'CardCost')

const skillIconDecoder = JsonDecoder.oneOf<SkillIcon>([
  JsonDecoder.object({ contents: JsonDecoder.string, tag: JsonDecoder.isExactly("SkillIcon") }, 'SkillIcon'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("WildIcon") }, 'WildIcon'),
  JsonDecoder.object({ tag: JsonDecoder.isExactly("WildMinusIcon") }, 'WildMinusIcon')
], 'SkillIcon')

export const cardDefDecoder = JsonDecoder.object<CardDef>(
  {
    art: JsonDecoder.string,
    level: JsonDecoder.nullable(JsonDecoder.number),
    cardType: JsonDecoder.string,
    cardCode: JsonDecoder.string,
    classSymbols: JsonDecoder.array<string>(JsonDecoder.string, 'string[]'),
    cardTraits: JsonDecoder.array<string>(JsonDecoder.string, 'string[]'),
    skills: JsonDecoder.array<SkillIcon>(skillIconDecoder, 'SkillIcon[]'),
    name: nameDecoder,
    cost: JsonDecoder.nullable(cardCostDecoder),
  },
  'CardDef',
);
