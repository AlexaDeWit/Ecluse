import json
import sys
import collections

transcript_path = "/home/alexa/.gemini/antigravity-cli/brain/c6e23b16-86fb-4aef-bf82-659e75a6d620/.system_generated/logs/transcript_full.jsonl"
files = collections.defaultdict(list)

for line in open(transcript_path):
    try:
        data = json.loads(line)
        if "tool_calls" in data:
            for tc in data["tool_calls"]:
                name = tc.get("name")
                args = tc.get("arguments", {})
                
                if name == "default_api:write_to_file":
                    target = args.get("TargetFile", "")
                    content = args.get("CodeContent", "")
                    if target:
                        files[target] = content
                        
                elif name == "default_api:replace_file_content":
                    target = args.get("TargetFile", "")
                    target_content = args.get("TargetContent", "")
                    replacement = args.get("ReplacementContent", "")
                    if target and target in files:
                        files[target] = files[target].replace(target_content, replacement)
                        
                elif name == "default_api:multi_replace_file_content":
                    target = args.get("TargetFile", "")
                    chunks = args.get("ReplacementChunks", [])
                    if target and target in files:
                        for chunk in chunks:
                            files[target] = files[target].replace(chunk["TargetContent"], chunk["ReplacementContent"])
                            
    except Exception as e:
        pass

# Also check my current transcript for the latest modifications!
my_transcript = "/home/alexa/.gemini/antigravity-cli/brain/19c8815f-3819-4b4a-ad66-454e1f67e037/.system_generated/logs/transcript_full.jsonl"
for line in open(my_transcript):
    try:
        data = json.loads(line)
        if "tool_calls" in data:
            for tc in data["tool_calls"]:
                name = tc.get("name")
                args = tc.get("arguments", {})
                if name == "default_api:replace_file_content":
                    target = args.get("TargetFile", "")
                    target_content = args.get("TargetContent", "")
                    replacement = args.get("ReplacementContent", "")
                    if target and target in files:
                        files[target] = files[target].replace(target_content, replacement)
                elif name == "default_api:multi_replace_file_content":
                    target = args.get("TargetFile", "")
                    chunks = args.get("ReplacementChunks", [])
                    if target and target in files:
                        for chunk in chunks:
                            files[target] = files[target].replace(chunk["TargetContent"], chunk["ReplacementContent"])
    except Exception as e:
        pass

for target, content in files.items():
    if "Ecluse/Pilot/Export" in target or "Ecluse/Pilot/S3ExportSpec" in target:
        print(f"Recovering {target}")
        import os
        os.makedirs(os.path.dirname(target), exist_ok=True)
        with open(target, "w") as f:
            f.write(content)

